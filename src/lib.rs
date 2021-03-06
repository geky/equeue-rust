
#![cfg_attr(not(feature="std"), no_std)]

#![deny(missing_debug_implementations)]

// TODO remove this eventually
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]


use core::mem::size_of;
use core::mem::align_of;
use core::fmt;
use core::alloc::Layout;
use core::ptr;
use core::slice;
use core::marker::PhantomData;
use core::ops::Deref;
use core::ops::DerefMut;
use core::borrow::Borrow;
use core::borrow::BorrowMut;
use core::mem::forget;
use core::ptr::drop_in_place;
use core::mem::transmute;
use core::cmp;
use core::num::NonZeroUsize;
use core::num::TryFromIntError;
use core::fmt::Debug;
use core::iter;
use core::future::Future;
use core::pin::Pin;
use core::task::Poll;
use core::task::Context;
use core::task::Waker;
use core::task::RawWaker;
use core::task::RawWakerVTable;
use core::ops::Add;
use core::ops::Sub;
use core::convert::Infallible;
use core::mem::transmute_copy;

use either::Either;
use either::Left;
use either::Right;

mod util;
use util::*;

mod traits;
pub use traits::*;

// Allow overriding the system implementation with EQUEUE_SYS_PATH
pub mod sys {
    include!(env!("EQUEUE_SYS_PATH"));
}
use sys::*;


/// Default number of bits of precision to use for scheduling events, this
/// limits the number of significant digits used in long-term events in order
/// to create better bucketing and power consumption
///
/// So if you needed to schedule an event in 20 minutes, aka 1200000
/// milliseconds, aka 0b000100100100111110000000 if equeue is running
/// with a millisecond-based clock:
///
/// 000100100100111110000000
/// 000100000000000000000000 => 1-bit of precision => ~20-37.48 minutes
/// 000110000000000000000000 => 2-bits of precision => ~20-28.74 minutes
/// 000111100000000000000000 => 4-bits of precision => ~20-22.18 minutes
/// 000111111110000000000000 => 8-bits of precision => ~20-20.14 minutes
/// 000111111111111111100000 => 16-bits of precision => ~20-20.00052 minutes
/// 000111111111111111111111 => 32-bits of precision => ~20 minutes
///
/// This probably deserves more analysis, but some handwavey theorized
/// runtimes:
///
/// - precision = n   => O(n)
/// - precision = n/2 => O(sqrt(n))
/// - precision = 1   => O(log(n))
///
const PRECISION: u8 = {
    match option_env!("EQUEUE_PRECISION") {
        Some(precision) => parse_const_u8(precision),
        None => 6
    }
};


/// Event queue errors
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[non_exhaustive]
pub enum Error {
    NoMem,
    Timeout,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::NoMem    => write!(f, "Out of memory"),
            Error::Timeout  => write!(f, "A timeout occured"),
        }
    }
}


/// Reasons why dispatch could exit
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Dispatch {
    Timeout,
    Break,
}


/// An Instant-like tick wrapper with better memory footprint
#[derive(Copy, Clone, Eq, PartialEq)]
#[repr(transparent)]
struct Tick(utick);

impl Debug for Tick {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}??", self.uticks())
    }
}

impl Tick {
    const fn new(t: utick) -> Tick {
        Self(t)
    }

    const fn uticks(&self) -> utick {
        self.0
    }

    // we store some deltas as ticks to reuse memory, so we need this,
    // it's not worth the noise to create a union
    const fn as_delta(self) -> Option<Delta> {
        Delta::new(self.uticks() as itick)
    }
}

impl PartialOrd for Tick {
    fn partial_cmp(&self, other: &Tick) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Tick {
    fn cmp(&self, other: &Tick) -> cmp::Ordering {
        scmp(self.uticks(), other.uticks())
    }
}

// Note our Tick class is a bit odd, as it's behavior is specialized
//
// - Sub saturates to zero, allowing no negative deltas
// - Add ignores wrapping, which allows Tick to ignore overflows

impl Sub for Tick {
    type Output = Delta;

    fn sub(self, other: Tick) -> Delta {
        let delta = max(sdiff(self.uticks(), other.uticks()), 0);
        unsafe { Delta::new_unchecked(delta) }
    }
}

impl Add<Delta> for Tick {
    type Output = Tick;

    fn add(self, other: Delta) -> Tick {
        Tick::new(self.uticks().wrapping_add(other.uticks()))
    }
}

impl Tick {
    fn imprecise_add(self, other: Delta, precision: u8) -> Tick {
        // In order to limit precision (which leads to more efficient data
        // structures and fewer wakeups/power consumption), we round up to
        // latest deadline in the configured precision. This just means
        // oring any bits lower than the precision with 1.
        //
        // Note that this always ensures a later deadline.
        let mask = (1 << (
            (8*size_of::<utick>() as u8).saturating_sub(
                other.uticks().leading_zeros() as u8 + precision
            )
        )) - 1;

        Tick::new(self.uticks().wrapping_add(other.uticks()) | mask)
    }
}

/// An Duration-like tick wrapper with better memory footprint
///
/// We store this inverted to take advantage of zero niches
#[derive(Copy, Clone, Eq, PartialEq)]
#[repr(transparent)]
pub struct Delta(NonZeroItick);

impl Debug for Delta {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}??", self.uticks())
    }
}

impl Delta {
    const _ASSERT_NICHE: bool = {
        assert!(size_of::<Option<Delta>>() == size_of::<itick>());
        true
    };

    #[inline]
    pub const fn zero() -> Delta {
        debug_assert!(Self::_ASSERT_NICHE);
        Self(unsafe { NonZeroItick::new_unchecked(!0) })
    }

    #[inline]
    pub const fn new(t: itick) -> Option<Delta> {
        debug_assert!(Self::_ASSERT_NICHE);
        if t >= 0 {
            Some(unsafe { Self::new_unchecked(t) })
        } else {
            None
        }
    }

    #[inline]
    pub const unsafe fn new_unchecked(t: itick) -> Delta {
        debug_assert!(Self::_ASSERT_NICHE);
        Self(NonZeroItick::new_unchecked(!t))
    }

    #[inline]
    pub const fn iticks(self) -> itick {
        !self.0.get()
    }

    #[inline]
    pub const fn uticks(self) -> utick {
        // completely safe since we know we have no negative values
        self.iticks() as utick
    }

    // we store some deltas as ticks to reuse memory, so we need this,
    // it's not worth the noise to create a union
    #[inline]
    const fn as_tick(self) -> Tick {
        Tick::new(self.uticks())
    }
}

impl PartialOrd for Delta {
    #[inline]
    fn partial_cmp(&self, other: &Delta) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Delta {
    #[inline]
    fn cmp(&self, other: &Delta) -> cmp::Ordering {
        self.uticks().cmp(&other.uticks())
    }
}

impl TryIntoDelta for Delta {
    type Error = Infallible;
    #[inline]
    fn try_into_delta(self, _: utick) -> Result<Delta, Self::Error> {
        Ok(self)
    }
}

impl TryFromDelta for Delta {
    type Error = Infallible;
    #[inline]
    fn try_from_delta(delta: Delta, _: utick) -> Result<Delta, Self::Error> {
        Ok(delta)
    }
}


// Generic atomic storage
trait AtomicStorage {
    type U: Copy;
    fn new(v: Self::U) -> Self;
    fn load(&self) -> Self::U;
    fn store(&self, v: Self::U);

    #[cfg(any(
        equeue_queue_mode="lockless",
        equeue_alloc_mode="lockless",
        equeue_break_mode="lockless",
    ))]
    fn cas(&self, old: Self::U, new: Self::U) -> Result<Self::U, Self::U>;
}

impl AtomicStorage for AtomicUdeptr {
    type U = udeptr;

    fn new(v: udeptr) -> Self {
        AtomicUdeptr::new(v)
    }

    fn load(&self) -> udeptr {
        self.load(Ordering::SeqCst)
    }

    fn store(&self, v: udeptr) {
        self.store(v, Ordering::SeqCst)
    }

    #[cfg(any(
        equeue_queue_mode="lockless",
        equeue_alloc_mode="lockless",
        equeue_break_mode="lockless",
    ))]
    fn cas(&self, old: udeptr, new: udeptr) -> Result<udeptr, udeptr> {
        self.compare_exchange(old, new, Ordering::SeqCst, Ordering::SeqCst)
    }
}

impl AtomicStorage for AtomicUeptr {
    type U = ueptr;

    fn new(v: ueptr) -> Self {
        AtomicUeptr::new(v)
    }

    fn load(&self) -> ueptr {
        self.load(Ordering::SeqCst)
    }

    fn store(&self, v: ueptr) {
        self.store(v, Ordering::SeqCst)
    }

    #[cfg(any(
        equeue_queue_mode="lockless",
        equeue_alloc_mode="lockless",
        equeue_break_mode="lockless",
    ))]
    fn cas(&self, old: ueptr, new: ueptr) -> Result<ueptr, ueptr> {
        self.compare_exchange(old, new, Ordering::SeqCst, Ordering::SeqCst)
    }
}

/// Small wrapper to generalize atomics to any <= udeptr sized type
#[repr(transparent)]
struct Atomic<T, S: AtomicStorage>(S, PhantomData<T>);

impl<T: Copy + Debug, S: AtomicStorage> Debug for Atomic<T, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Atomic")
            .field(&self.load())
            .finish()
    }
}

impl<T: Copy, S: AtomicStorage> Atomic<T, S> {
    const _ASSERT_FITS: bool = {
        assert!(size_of::<T>() <= size_of::<S::U>());
        true
    };

    fn new(t: T) -> Self {
        // compile-time assert
        debug_assert!(Self::_ASSERT_FITS);

        Self(
            S::new(unsafe { transmute_copy(&t) }),
            PhantomData
        )
    }

    fn load(&self) -> T {
        unsafe { transmute_copy(&self.0.load()) }
    }

    fn store(&self, t: T) {
        self.0.store(unsafe { transmute_copy(&t) })
    }

    #[cfg(any(
        equeue_queue_mode="lockless",
        equeue_alloc_mode="lockless",
        equeue_break_mode="lockless",
    ))]
    fn cas(&self, old: T, new: T) -> Result<T, T> {
        self.0.cas(
            unsafe { transmute_copy(&old) },
            unsafe { transmute_copy(&new) },
        )
            .map(|t| unsafe { transmute_copy(&t) })
            .map_err(|t| unsafe { transmute_copy(&t) })
    }
}


/// Slab-internal pointer, with internalized generation count
#[repr(transparent)]
struct EPtr<T>(ueptr, PhantomData<*const T>);

impl<T> Debug for EPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // these really need to be in hex to be readable
        f.debug_tuple("EPtr")
            .field(&format_args!("{:#x}", self.0))
            .finish()
    }
}

impl<T> Clone for EPtr<T> {
    fn clone(&self) -> EPtr<T> {
        EPtr(self.0, self.1)
    }
}

impl<T> Copy for EPtr<T> {}

impl<T> PartialEq for EPtr<T> {
    fn eq(&self, other: &EPtr<T>) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for EPtr<T> {}

unsafe impl<T> Send for EPtr<T> {}
unsafe impl<T> Sync for EPtr<T> {}

impl<T> EPtr<T> {
    const fn null() -> EPtr<T> {
        EPtr(0, PhantomData)
    }

    fn from_ref<C>(q: &Equeue<C>, e: &T) -> EPtr<T> {
        if e as *const _ == &q.queue as *const _ as *const _ {
            EPtr(1, PhantomData)
        } else if e as *const _ == &q.dequeue as *const _ as *const _ {
            EPtr(2, PhantomData)
        } else {
            EPtr(
                unsafe {
                    ((e as *const _ as *const u8)
                        .offset_from(q.slab.as_ptr())
                        as usize
                        / size_of::<udeptr>())
                        as ueptr
                },
                PhantomData
            )
        }
    }

    fn as_ptr<C>(self, q: &Equeue<C>) -> *const T {
        match self.0 {
            0 => ptr::null(),
            1 => &q.queue as *const _ as *const T,
            2 => &q.dequeue as *const _ as *const T,
            _ => (
                &q.slab[self.0 as usize * size_of::<udeptr>()]
                    as *const _ as *const T
            )
        }
    }

    fn as_ref<'a, C>(self, q: &'a Equeue<C>) -> Option<&'a T> {
        unsafe { self.as_ptr(q).as_ref() }
    }
}

/// A marked eptr, used to double-check non-locking parts
/// of the data structures
#[repr(C)] // we need a specific order to transmute between marked types
struct MarkedEPtr<T> {
    gen: ugen,
    mark: ugen,
    eptr: EPtr<T>,
}

impl<T> Debug for MarkedEPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // these really need to be in hex to be readable
        f.debug_tuple("MarkedEPtr")
            .field(&self.gen)
            .field(&self.mark)
            .field(&format_args!("{:#x}", self.eptr.0))
            .finish()
    }
}

impl<T> Clone for MarkedEPtr<T> {
    fn clone(&self) -> Self {
        MarkedEPtr {
            gen: self.gen,
            mark: self.mark,
            eptr: self.eptr,
        }
    }
}

impl<T> Copy for MarkedEPtr<T> {}

impl<T> PartialEq for MarkedEPtr<T> {
    fn eq(&self, other: &MarkedEPtr<T>) -> bool {
        self.gen == other.gen
            && self.mark == other.mark
            && self.eptr == other.eptr
    }
}

impl<T> Eq for MarkedEPtr<T> {}

impl<T> MarkedEPtr<T> {
    const fn null() -> MarkedEPtr<T> {
        MarkedEPtr {
            gen: 0,
            mark: 0,
            eptr: EPtr::null(),
        }
    }

    fn as_ptr<C>(self, q: &Equeue<C>) -> *const T {
        self.eptr.as_ptr(q)
    }

    fn as_ref<'a, C>(self, q: &'a Equeue<C>) -> Option<&'a T> {
        self.eptr.as_ref(q)
    }

    fn inc(self) -> Self {
        MarkedEPtr {
            gen: self.gen.wrapping_add(1),
            mark: self.mark,
            eptr: self.eptr,
        }
    }

    fn set_eptr(self, other: EPtr<T>) -> MarkedEPtr<T> {
        MarkedEPtr {
            gen: self.gen,
            mark: self.mark,
            eptr: other,
        }
    }

    fn cp_eptr(self, other: MarkedEPtr<T>) -> MarkedEPtr<T> {
        MarkedEPtr {
            gen: self.gen,
            mark: self.mark,
            eptr: other.eptr,
        }
    }

    fn set_mark(self, mark: ugen) -> MarkedEPtr<T> {
        MarkedEPtr {
            gen: self.gen,
            mark: mark,
            eptr: self.eptr,
        }
    }

    fn as_marked<U>(self) -> MarkedEPtr<U> {
        unsafe { transmute_copy(&self) }
    }

    fn as_info(self) -> EInfo {
        unsafe { transmute_copy(&self) }
    }
}

// interactions with atomics
impl<T, S: AtomicStorage> Atomic<MarkedEPtr<T>, S> {
    fn store_marked(&self, old: MarkedEPtr<T>, new: EPtr<T>) -> MarkedEPtr<T> {
        let new = old.set_eptr(new);
        self.store(new);
        new
    }

    fn store_marked_inc(&self, old: MarkedEPtr<T>, new: EPtr<T>) -> MarkedEPtr<T> {
        let new = old.set_eptr(new).inc();
        self.store(new);
        new
    }

    #[cfg(any(
        equeue_queue_mode="lockless",
        equeue_alloc_mode="lockless",
    ))]
    fn cas_marked(&self, old: MarkedEPtr<T>, new: EPtr<T>) -> Result<MarkedEPtr<T>, MarkedEPtr<T>> {
        let new = old.set_eptr(new);
        self.cas(old, new)
    }

    #[cfg(any(
        equeue_queue_mode="lockless",
        equeue_alloc_mode="lockless",
    ))]
    fn cas_marked_inc(&self, old: MarkedEPtr<T>, new: EPtr<T>) -> Result<MarkedEPtr<T>, MarkedEPtr<T>> {
        let new = old.set_eptr(new).inc();
        self.cas(old, new)
    }

    fn as_eptr<C>(&self, q: &Equeue<C>) -> EPtr<Atomic<MarkedEPtr<T>, S>> {
        EPtr::from_ref(q, self)
    }

    fn as_atom<'a>(&'a self) -> &'a Atomic<Marked, S> {
        unsafe { &*(self as *const _ as *const Atomic<Marked, S>) }
    }
}

// this is just an alias for a generic marked thing
type Marked = MarkedEPtr<()>;


/// Several event fields are crammed in here to avoid wasting space
#[derive(Copy, Clone, Eq, PartialEq)]
#[repr(C)] // we need a specific order to transmute between marked types
struct EInfo {
    gen: ugen,
    id: ugen,
    state: u8,
    npw2: u8,
}

impl Debug for EInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EInfo")
            .field("gen", &self.gen)
            .field("id", &self.id)
            .field("state", &self.state())
            .field("static", &self.static_())
            .field("once", &self.once())
            .field("npw2", &self.npw2)
            .finish()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum EState {
    Alloced  = 0,
    InQueue  = 1,
    InFlight = 2,
    Nested   = 3,
    Canceled = 4,
}

impl EState {
    fn from_u8(state: u8) -> EState {
        match state {
            0 => EState::Alloced,
            1 => EState::InQueue,
            2 => EState::InFlight,
            3 => EState::Nested,
            4 => EState::Canceled,
            _ => unreachable!(),
        }
    }

    fn as_u8(&self) -> u8 {
        match self {
            EState::Alloced  => 0,
            EState::InQueue  => 1,
            EState::InFlight => 2,
            EState::Nested   => 3,
            EState::Canceled => 4,
        }
    }
}

impl EInfo {
    fn new(id: ugen, state: EState, static_: bool, once: bool, npw2: u8) -> EInfo {
        EInfo {
            gen: 0,
            id: id,
            state: (
                ((static_ as u8) << 7)
                | ((once as u8) << 6)
                | state.as_u8()
            ),
            npw2: npw2,
        }
    }

    fn static_(&self) -> bool {
        self.state & 0x80 != 0
    }

    fn once(&self) -> bool {
        self.state & 0x40 != 0
    }

    fn state(&self) -> EState {
        EState::from_u8(self.state & 0x0f)
    }

    fn inc_id(self) -> EInfo {
        EInfo {
            gen: self.gen,
            id: self.id.wrapping_add(1),
            state: self.state,
            npw2: self.npw2,
        }
    }

    fn set_static(self, static_: bool) -> EInfo {
        EInfo {
            gen: self.gen,
            id: self.id,
            state: if static_ {
                self.state | 0x80
            } else {
                self.state & !0x80
            },
            npw2: self.npw2,
        }
    }

    fn set_once(self, static_: bool) -> EInfo {
        EInfo {
            gen: self.gen,
            id: self.id,
            state: if static_ {
                self.state | 0x40
            } else {
                self.state & !0x40
            },
            npw2: self.npw2,
        }
    }

    fn set_state(self, state: EState) -> EInfo {
        EInfo {
            gen: self.gen,
            id: self.id,
            state: (self.state & !0x0f) | state.as_u8(),
            npw2: self.npw2,
        }
    }

    fn as_marked<U>(self) -> MarkedEPtr<U> {
        unsafe { transmute_copy(&self) }
    }
}

// interactions with atomics
impl<S: AtomicStorage> Atomic<EInfo, S> {
    fn as_atom<'a>(&'a self) -> &'a Atomic<Marked, S> {
        unsafe { &*(self as *const _ as *const Atomic<Marked, S>) }
    }
}


/// Internal event header
#[derive(Debug)]
struct EBuf {
    next: Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>,
    next_back: Atomic<MarkedEPtr<Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>>, AtomicUdeptr>,
    sibling: Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>,
    sibling_back: Atomic<MarkedEPtr<Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>>, AtomicUdeptr>,
    info: Atomic<EInfo, AtomicUdeptr>,

    cb: Option<fn(*mut u8)>,
    drop: Option<fn(*mut u8)>,
    target: Tick,
    period: Option<Delta>,
    // TODO can we store this somewhere else?
    q: *const Equeue<()>,
}

impl EBuf {
    const ALIGN: usize = {
        let align = max_const_usize(
            align_of::<EBuf>(),
            size_of::<udeptr>(),
        );
        assert!(align >= align_of::<*const ()>());
        assert!(align >= align_of::<AtomicUdeptr>());
        align
    };

    fn as_eptr<C>(&self, q: &Equeue<C>) -> EPtr<EBuf> {
        EPtr::from_ref(q, self)
    }

    // info access
    fn npw2(&self) -> u8 {
        self.info.load().npw2
    }

    fn size(&self) -> usize {
        EBuf::ALIGN << self.npw2()
    }

    // access to the trailing buffer
    fn data_ptr<T>(&self) -> *const T {
        unsafe { (self as *const EBuf).add(1) as *const T }
    }

    fn data_mut_ptr<T>(&mut self) -> *mut T {
        unsafe { (self as *mut EBuf).add(1) as *mut T }
    }

    unsafe fn data_ref<'a, T>(&'a self) -> &'a T {
        &*self.data_ptr()
    }

    unsafe fn data_mut<'a, T>(&'a mut self) -> &'a mut T {
        &mut *self.data_mut_ptr()
    }

    unsafe fn from_data_mut_ptr<'a, T: ?Sized + 'a>(ptr: *mut T) -> Option<&'a mut EBuf> {
        ptr.as_mut()
            .map(|ref_| EBuf::from_data_mut(ref_))
    }

    unsafe fn from_data_mut<'a, T: ?Sized>(ref_: &'a mut T) -> &'a mut EBuf {
        &mut *(ref_ as *mut _ as *mut EBuf).sub(1)
    }

    // we can "claim" an EBuf once we remove it from shared structures,
    // the claim is unsafe, but afterwards we can leverage Rust's type
    // system to know whether or not we have exclusive access to the EBuf
    unsafe fn claim<'a>(&'a self) -> &'a mut EBuf {
        &mut *(self as *const EBuf as *mut EBuf)
    }
}

// some convenience extensions to Option<&EBuf>
trait AsEPtr {
    type Target;
    fn as_eptr<C>(&self, q: &Equeue<C>) -> EPtr<Self::Target>;
}

impl AsEPtr for Option<&EBuf> {
    type Target = EBuf;
    fn as_eptr<C>(&self, q: &Equeue<C>) -> EPtr<Self::Target> {
        match self {
            Some(e) => e.as_eptr(q),
            None => EPtr::null(),
        }
    }
}

impl<T> AsEPtr for Option<&Atomic<MarkedEPtr<T>, AtomicUdeptr>> {
    type Target = Atomic<MarkedEPtr<T>, AtomicUdeptr>;
    fn as_eptr<C>(&self, q: &Equeue<C>) -> EPtr<Self::Target> {
        match self {
            Some(e) => e.as_eptr(q),
            None => EPtr::null(),
        }
    }
}

/// EState machine for mutually synchronized operations
#[cfg(equeue_queue_mode="lockless")]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
enum HelpState {
    Done                             = 0,  // <------.
                                           //        |
    EnqueueSliceNextBackNext         = 1,  // -.     |
    EnqueueSliceNextNextBack         = 2,  // <'---. |
    EnqueueSiblingSiblingSiblingBack = 3,  // -.   | |
    EnqueueSiblingSiblingBackSibling = 4,  // <'---+ |
                                           //      | |
    UnqueueSliceNextBackNext         = 5,  // -.   | |
    UnqueueSliceNextNextBack         = 6,  // <'-. | |
    UnqueueSiblingSiblingNext        = 7,  // -. | | |
    UnqueueSiblingSiblingNextBack    = 8,  // <' | | |
    UnqueueSiblingNextBackNext       = 9,  // <' | | |
    UnqueueSiblingNextNextBack       = 10, // <' | | |
    UnqueueSiblingBackSibling        = 11, // <'<' | |
    UnqueueSiblingSiblingBack        = 12, // <'   | |
    UnqueueNext                      = 13, // <'---+ |
                                           //      | |
    DequeueDequeue                   = 14, // -.   | |
    DequeueQueue                     = 15, // <'   | |
    DequeueNextBack                  = 16, // <'   | |
    DequeueBackNextNextBack          = 17, // <'   | |
    DequeueBackNext                  = 18, // <'---|-+
                                           //      | |
    UpdateState                      = 19, // <----'-'
}

#[cfg(equeue_queue_mode="lockless")]
impl HelpState {
    fn from_u8(state: u8) -> HelpState {
        match state {
            0  => HelpState::Done,
            1  => HelpState::EnqueueSliceNextBackNext,
            2  => HelpState::EnqueueSliceNextNextBack,
            3  => HelpState::EnqueueSiblingSiblingSiblingBack,
            4  => HelpState::EnqueueSiblingSiblingBackSibling,
            5  => HelpState::UnqueueSliceNextBackNext,
            6  => HelpState::UnqueueSliceNextNextBack,
            7  => HelpState::UnqueueSiblingSiblingNext,
            8  => HelpState::UnqueueSiblingSiblingNextBack,
            9  => HelpState::UnqueueSiblingNextBackNext,
            10 => HelpState::UnqueueSiblingNextNextBack,
            11 => HelpState::UnqueueSiblingBackSibling,
            12 => HelpState::UnqueueSiblingSiblingBack,
            13 => HelpState::UnqueueNext,
            14 => HelpState::DequeueDequeue,
            15 => HelpState::DequeueQueue,
            16 => HelpState::DequeueNextBack,
            17 => HelpState::DequeueBackNextNextBack,
            18 => HelpState::DequeueBackNext,
            19 => HelpState::UpdateState,
            _  => unreachable!(),
        }
    }

    fn as_u8(&self) -> u8 {
        match self {
            HelpState::Done                             => 0,
            HelpState::EnqueueSliceNextBackNext         => 1,
            HelpState::EnqueueSliceNextNextBack         => 2,
            HelpState::EnqueueSiblingSiblingSiblingBack => 3,
            HelpState::EnqueueSiblingSiblingBackSibling => 4,
            HelpState::UnqueueSliceNextBackNext         => 5,
            HelpState::UnqueueSliceNextNextBack         => 6,
            HelpState::UnqueueSiblingSiblingNext        => 7,
            HelpState::UnqueueSiblingSiblingNextBack    => 8,
            HelpState::UnqueueSiblingNextBackNext       => 9,
            HelpState::UnqueueSiblingNextNextBack       => 10,
            HelpState::UnqueueSiblingBackSibling        => 11,
            HelpState::UnqueueSiblingSiblingBack        => 12,
            HelpState::UnqueueNext                      => 13,
            HelpState::DequeueDequeue                   => 14,
            HelpState::DequeueQueue                     => 15,
            HelpState::DequeueNextBack                  => 16,
            HelpState::DequeueBackNextNextBack          => 17,
            HelpState::DequeueBackNext                  => 18,
            HelpState::UpdateState                      => 19,
        }
    }
}

#[cfg(equeue_queue_mode="lockless")]
#[derive(Copy, Clone)]
struct HelpOp {
    gen: ugen,
    state: u8,
    eptr: EPtr<EBuf>,
}

#[cfg(equeue_queue_mode="lockless")]
impl Debug for HelpOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HelpOp")
            .field("gen", &self.gen)
            .field("state", &self.state())
            .field("eptr", &self.eptr)
            .field("estate", &self.estate())
            .finish()
    }
}

#[cfg(equeue_queue_mode="lockless")]
impl HelpOp {
    const fn done() -> HelpOp {
        HelpOp {
            gen: 0,
            state: 0,
            eptr: EPtr::null(),
        }
    }

    fn is_done(&self) -> bool {
        self.state() == HelpState::Done
    }

    fn state(&self) -> HelpState {
        HelpState::from_u8(self.state & 0x1f)
    }

    fn estate(&self) -> EState {
        EState::from_u8(self.state >> 5)
    }

    fn set_state(self, state: HelpState) -> HelpOp {
        HelpOp {
            gen: self.gen,
            state: (self.state & !0x1f) | state.as_u8(),
            eptr: self.eptr,
        }
    }

    fn set_estate(self, estate: EState) -> HelpOp {
        HelpOp {
            gen: self.gen,
            state: (self.state & !0xe0) | (estate.as_u8() << 5),
            eptr: self.eptr,
        }
    }

    fn set_eptr(self, eptr: EPtr<EBuf>) -> HelpOp {
        HelpOp {
            gen: self.gen,
            state: self.state,
            eptr: eptr,
        }
    }

    fn inc(self) -> HelpOp {
        HelpOp {
            gen: self.gen.wrapping_add(1),
            state: self.state,
            eptr: self.eptr,
        }
    }

    fn atom<'a, C>(
        &self,
        q: &'a Equeue<C>
    ) -> Option<&'a Atomic<Marked, AtomicUdeptr>> {
        match self.state() {
            HelpState::Done => unreachable!(),
            HelpState::EnqueueSliceNextBackNext => {
                self.eptr.as_ref(q).unwrap()
                    .next_back.load().as_ref(q)
                    .map(|next_back| next_back.as_atom())
            }
            HelpState::EnqueueSliceNextNextBack => {
                self.eptr.as_ref(q).unwrap()
                    .next.load().as_ref(q)
                    .map(|e| e.next_back.as_atom())
            }
            HelpState::EnqueueSiblingSiblingSiblingBack => {
                self.eptr.as_ref(q).unwrap()
                    .sibling.load().as_ref(q)
                    .map(|e| e.sibling_back.as_atom())
            }
            HelpState::EnqueueSiblingSiblingBackSibling => {
                self.eptr.as_ref(q).unwrap()
                    .sibling_back.load().as_ref(q)
                    .map(|sibling_back| sibling_back.as_atom())
            }
            HelpState::UnqueueSliceNextBackNext => {
                self.eptr.as_ref(q).unwrap()
                    .next_back.load().as_ref(q)
                    .map(|next_back| next_back.as_atom())
            }
            HelpState::UnqueueSliceNextNextBack => {
                self.eptr.as_ref(q).unwrap()
                    .next.load().as_ref(q)
                    .map(|e| e.next_back.as_atom())
            }
            HelpState::UnqueueSiblingSiblingNext => {
                self.eptr.as_ref(q).unwrap()
                    .sibling.load().as_ref(q)
                    .map(|e| e.next.as_atom())
            }
            HelpState::UnqueueSiblingSiblingNextBack => {
                self.eptr.as_ref(q).unwrap()
                    .sibling.load().as_ref(q)
                    .map(|e| e.next_back.as_atom())
            }
            HelpState::UnqueueSiblingNextBackNext => {
                self.eptr.as_ref(q).unwrap()
                    .next_back.load().as_ref(q)
                    .map(|next_back| next_back.as_atom())
            }
            HelpState::UnqueueSiblingNextNextBack => {
                self.eptr.as_ref(q).unwrap()
                    .next.load().as_ref(q)
                    .map(|e| e.next_back.as_atom())
            }
            HelpState::UnqueueSiblingBackSibling => {
                self.eptr.as_ref(q).unwrap()
                    .sibling_back.load().as_ref(q)
                    .map(|sibling_back| sibling_back.as_atom())
            }
            HelpState::UnqueueSiblingSiblingBack => {
                self.eptr.as_ref(q).unwrap()
                    .sibling.load().as_ref(q)
                    .map(|e| e.sibling_back.as_atom())
            }
            HelpState::UnqueueNext => {
                Some(self.eptr.as_ref(q).unwrap().next.as_atom())
            }
            HelpState::DequeueDequeue => {
                Some(q.dequeue.as_atom())
            }
            HelpState::DequeueQueue => {
                Some(q.queue.as_atom())
            }
            HelpState::DequeueNextBack => {
                q.dequeue.load().as_ref(q)
                    .map(|e| e.next_back.as_atom())
            }
            HelpState::DequeueBackNextNextBack => {
                self.eptr.as_ref(q).unwrap()
                    .next.load().as_ref(q)
                    .map(|e| e.next_back.as_atom())
            }
            HelpState::DequeueBackNext => {
                Some(self.eptr.as_ref(q).unwrap().next.as_atom())
            }
            HelpState::UpdateState => {
                Some(self.eptr.as_ref(q).unwrap().info.as_atom())
            }
        }
    }

    fn new<C>(
        &self,
        q: &Equeue<C>,
        help_old: Marked
    ) -> Marked {
        match self.state() {
            HelpState::Done => unreachable!(),
            HelpState::EnqueueSliceNextBackNext => {
                help_old.as_marked::<EBuf>()
                    .set_eptr(self.eptr)
                    .as_marked().inc()
            }
            HelpState::EnqueueSliceNextNextBack => {
                help_old.as_marked::<Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>>()
                    .set_eptr(self.eptr.as_ref(q).unwrap().next.as_eptr(q))
                    .as_marked().inc()
            }
            HelpState::EnqueueSiblingSiblingSiblingBack => {
                help_old.as_marked::<Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>>()
                    .set_eptr(self.eptr.as_ref(q).unwrap().sibling.as_eptr(q))
                    .as_marked().inc()
            }
            HelpState::EnqueueSiblingSiblingBackSibling => {
                help_old.as_marked::<EBuf>()
                    .set_eptr(self.eptr)
                    .as_marked().inc()
            }
            HelpState::UnqueueSliceNextBackNext => {
                help_old.as_marked::<EBuf>()
                    .cp_eptr(self.eptr.as_ref(q).unwrap().next.load())
                    .as_marked().inc()
            }
            HelpState::UnqueueSliceNextNextBack => {
                help_old.as_marked::<Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>>()
                    .cp_eptr(self.eptr.as_ref(q).unwrap().next_back.load())
                    .as_marked().inc()
            }
            HelpState::UnqueueSiblingSiblingNext => {
                help_old.as_marked::<EBuf>()
                    .cp_eptr(self.eptr.as_ref(q).unwrap().next.load())
                    .as_marked().inc()
            }
            HelpState::UnqueueSiblingSiblingNextBack => {
                help_old.as_marked::<Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>>()
                    .cp_eptr(self.eptr.as_ref(q).unwrap().next_back.load())
                    .as_marked().inc()
            }
            HelpState::UnqueueSiblingNextBackNext => {
                help_old.as_marked::<EBuf>()
                    .cp_eptr(self.eptr.as_ref(q).unwrap().sibling.load())
                    .as_marked().inc()
            }
            HelpState::UnqueueSiblingNextNextBack => {
                help_old.as_marked::<Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>>()
                    .set_eptr(
                        self.eptr.as_ref(q).unwrap().sibling.load().as_ref(q)
                            .map(|sibling| sibling.next.as_eptr(q))
                            .unwrap_or(EPtr::null())
                    )
                    .as_marked().inc()
            }
            HelpState::UnqueueSiblingBackSibling => {
                help_old.as_marked::<EBuf>()
                    .cp_eptr(self.eptr.as_ref(q).unwrap().sibling.load())
                    .as_marked().inc()
            }
            HelpState::UnqueueSiblingSiblingBack => {
                help_old.as_marked::<Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>>()
                    .cp_eptr(self.eptr.as_ref(q).unwrap().sibling_back.load())
                    .as_marked().inc()
            }
            HelpState::UnqueueNext => {
                help_old.as_marked::<EBuf>()
                    .set_eptr(EPtr::null())
                    .as_marked().inc()
            }
            // The only reason we need a redundant mark here for queue and dequeue
            // is so that we can invalidate traversals correctly. If we only had
            // a mark in dequeue, it's possible to get the updated dequeue mark,
            // but the outdated queue pointer. Keeping a mark in both avoids this.
            //
            // You could also reverse the order of queue and dequeue updates,
            // but then we'd need to store the old queue pointer somewhere, which
            // doesn't work with this FSM scheme.
            //
            HelpState::DequeueDequeue => {
                help_old.as_marked::<EBuf>()
                    .set_mark(help_old.mark.wrapping_add(1))
                    .cp_eptr(q.queue.load())
                    .as_marked().inc()
            }
            HelpState::DequeueQueue => {
                help_old.as_marked::<EBuf>()
                    .set_mark(help_old.mark.wrapping_add(1))
                    .cp_eptr(self.eptr.as_ref(q).unwrap().next.load())
                    .as_marked().inc()
            }
            HelpState::DequeueNextBack => {
                help_old.as_marked::<Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>>()
                    .set_eptr(q.dequeue.as_eptr(q))
                    .as_marked().inc()
            }
            HelpState::DequeueBackNextNextBack => {
                help_old.as_marked::<Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>>()
                    .set_eptr(q.queue.as_eptr(q))
                    .as_marked().inc()
            }
            HelpState::DequeueBackNext => {
                help_old.as_marked::<EBuf>()
                    .set_eptr(EPtr::null())
                    .as_marked().inc()
            }
            HelpState::UpdateState => {
                help_old.as_info()
                    .set_state(self.estate())
                    .as_marked().inc()
            }
        }
    }

    fn next(&self) -> HelpOp {
        let state_ = match self.state() {
            HelpState::Done                             => HelpState::Done,                             // <------.
                                                                                                        //        |
            HelpState::EnqueueSliceNextBackNext         => HelpState::EnqueueSliceNextNextBack,         // -.     |
            HelpState::EnqueueSliceNextNextBack         => HelpState::UpdateState,                      // <'---. |
            HelpState::EnqueueSiblingSiblingSiblingBack => HelpState::EnqueueSiblingSiblingBackSibling, // -.   | |
            HelpState::EnqueueSiblingSiblingBackSibling => HelpState::UpdateState,                      // <'---+ |
                                                                                                        //      | |
            HelpState::UnqueueSliceNextBackNext         => HelpState::UnqueueSliceNextNextBack,         // -.   | |
            HelpState::UnqueueSliceNextNextBack         => HelpState::UnqueueSiblingBackSibling,        // <'-. | |
            HelpState::UnqueueSiblingSiblingNext        => HelpState::UnqueueSiblingSiblingNextBack,    // -. | | |
            HelpState::UnqueueSiblingSiblingNextBack    => HelpState::UnqueueSiblingNextBackNext,       // <' | | |
            HelpState::UnqueueSiblingNextBackNext       => HelpState::UnqueueSiblingNextNextBack,       // <' | | |
            HelpState::UnqueueSiblingNextNextBack       => HelpState::UnqueueSiblingBackSibling,        // <' | | |
            HelpState::UnqueueSiblingBackSibling        => HelpState::UnqueueSiblingSiblingBack,        // <'<' | |
            HelpState::UnqueueSiblingSiblingBack        => HelpState::UnqueueNext,                      // <'   | |
            HelpState::UnqueueNext                      => HelpState::UpdateState,                      // <'---+ |
                                                                                                        //      | |
            HelpState::DequeueDequeue                   => HelpState::DequeueQueue,                     // -.   | |
            HelpState::DequeueQueue                     => HelpState::DequeueNextBack,                  // <'   | |
            HelpState::DequeueNextBack                  => HelpState::DequeueBackNextNextBack,          // <'   | |
            HelpState::DequeueBackNextNextBack          => HelpState::DequeueBackNext,                  // <'   | |
            HelpState::DequeueBackNext                  => HelpState::Done,                             // <'---|-+
                                                                                                        //      | |
            HelpState::UpdateState                      => HelpState::Done,                             // <----'-'
        };

        if state_ == HelpState::Done { *self } else { self.inc() }
            .set_state(state_)
    }
}


/// Event queue struct
#[derive(Debug)]
pub struct Equeue<
    #[cfg(feature="std")] C=SysClock,
    #[cfg(not(feature="std"))] C,
> {
    // memory management
    slab: &'static [u8],
    slab_front: Atomic<ueptr, AtomicUeptr>,
    slab_back: Atomic<ueptr, AtomicUeptr>,
    #[cfg(feature="alloc")] alloced: bool,

    // queue management
    queue: Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>,
    dequeue: Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>,
    break_: Atomic<u8, AtomicUeptr>,
    precision: u8,

    #[cfg(equeue_queue_mode="lockless")] help_op: Atomic<HelpOp, AtomicUdeptr>,
    #[cfg(equeue_queue_mode="lockless")] help_ctx: Atomic<MarkedEPtr<Atomic<Marked, AtomicUdeptr>>, AtomicUdeptr>,

    // other things
    clock: C,
    #[cfg(any(
        equeue_queue_mode="locking",
        equeue_alloc_mode="locking",
        equeue_break_mode="locking",
    ))]
    lock: SysLock,
}

// assert that we implement Send + Sync
#[allow(unconditional_recursion)] fn assert_send<T: Send, C: Send>() -> ! { assert_send::<Equeue<C>, C>() }
#[allow(unconditional_recursion)] fn assert_sync<T: Sync, C: Sync>() -> ! { assert_sync::<Equeue<C>, C>() }

/// Event queue configuration
#[derive(Debug)]
pub struct Config<
    #[cfg(feature="std")] C=SysClock,
    #[cfg(not(feature="std"))] C=(),
> {
    // lazily allocate clock so we don't create it if we don't use it
    clock: Either<fn() -> C, C>,
    precision: u8,

    buffer: Option<Either<usize, &'static mut [u8]>>,
}

impl Config {
    pub fn new() -> Config {
        Config {
            #[cfg(feature="std")] clock: Left(|| SysClock::new()),
            #[cfg(not(feature="std"))] clock: Left(|| ()),
            precision: PRECISION,
            buffer: None,
        }
    }
}

impl<C> Config<C> {
    pub fn clock<C_>(self, clock: C_) -> Config<C_> {
        let Config { precision, buffer, .. } = self;
        Config {
            clock: Right(clock),
            precision,
            buffer,
        }
    }

    pub fn precision(self, precision: u8) -> Config<C> {
        let mut self_ = self;
        self_.precision = precision;
        self_
    }

    #[cfg(feature="alloc")]
    pub fn size(self, size: usize) -> Config<C> {
        let mut self_ = self;
        self_.buffer = Some(Left(size));
        self_
    }

    pub fn buffer(self, buffer: &'static mut [u8]) -> Config<C> {
        let mut self_ = self;
        self_.buffer = Some(Right(buffer));
        self_
    }
}

impl<C> Equeue<C> {
    pub fn with_config(config: Config<C>) -> Equeue<C> {
        let (buffer, alloced) = match config.buffer {
            #[cfg(feature="alloc")]
            Some(Left(size)) => {
                let size = aligndown(size, EBuf::ALIGN);
                let layout = Layout::from_size_align(size, EBuf::ALIGN).unwrap();
                let buffer = unsafe { alloc(layout) };
                assert!(!buffer.is_null());

                let buffer = unsafe { slice::from_raw_parts_mut(buffer, size) };
                (buffer, true)
            }
            Some(Right(buffer)) => {
                (buffer, false)
            }
            _ => {
                panic!("equeue: no buffer configured");
            }
        };

        // align buffer
        let range = buffer.as_ptr_range();
        let start = alignup(range.start as usize, EBuf::ALIGN) - range.start as usize;
        let end = aligndown(range.end as usize, EBuf::ALIGN) - range.start as usize;
        let buffer = buffer.get_mut(start..end).unwrap();

        // go ahead and zero our buffer, this makes it easier to manage bucket
        // allocation, which needs to be null the moment a bucket is allocated
        buffer.fill(0);

        Equeue {
            slab: buffer,
            #[cfg(feature="alloc")] alloced: alloced,
            // Go ahead and allocate 3 buckets, this is actually so we can
            // reserve eptrs 0,1,2 for null, queue, and dequeue
            slab_front: Atomic::new(3),
            slab_back: Atomic::new(ueptr::try_from(buffer.len() / size_of::<udeptr>()).unwrap()),

            queue: Atomic::new(MarkedEPtr::null()),
            dequeue: Atomic::new(MarkedEPtr::null()),
            break_: Atomic::new(0),
            precision: config.precision,

            #[cfg(equeue_queue_mode="lockless")] help_op: Atomic::new(HelpOp::done()),
            #[cfg(equeue_queue_mode="lockless")] help_ctx: Atomic::new(MarkedEPtr::null()),

            clock: config.clock
                .map_left(|f| f())
                .into_inner(),
            #[cfg(any(
                equeue_queue_mode="locking",
                equeue_alloc_mode="locking",
                equeue_break_mode="locking",
            ))]
            lock: SysLock::new(),
        }
    }
}

#[cfg(feature="std")]
impl Equeue {
    #[cfg(feature="alloc")]
    pub fn with_size(size: usize) -> Equeue {
        Equeue::with_config(Config::new().size(size))
    }

    pub fn with_buffer(buffer: &'static mut [u8]) -> Equeue {
        Equeue::with_config(Config::new().buffer(buffer))
    }
}

impl<C> Drop for Equeue<C> {
    fn drop(&mut self) {
        // make sure we call drop on any pending events
        //
        // we can't just traverse the queue here, because alloced but unpended
        // events aren't there until post is called, this includes
        // intermediary states for static events/futures.
        //
        // it's up to dealloc_ to make sure drop is cleared after called
        let mut i = self.slab_back.load() as usize * size_of::<udeptr>();
        while let Some(e) = self.slab.get(i) {
            let e = unsafe { &*(e as *const _ as *const EBuf) };
            let e = unsafe { e.claim() };

            if let Some(drop) = e.drop {
                drop(e.data_mut_ptr());
            }
            e.drop = None;

            // just some extra precautions against poorly written wakers
            e.info.store(e.info.load().inc_id());

            i = i.saturating_add(size_of::<EBuf>() + e.size());
        }

        // free allocated buffer?
        #[cfg(feature="alloc")]
        if self.alloced {
            let layout = Layout::from_size_align(self.slab.len(), EBuf::ALIGN).unwrap();
            unsafe { dealloc(self.slab.as_ptr() as *mut u8, layout) };
        }
    }
}

impl<C> Equeue<C> {
    fn contains(&self, e: &EBuf) -> bool {
        self.slab.as_ptr_range()
            .contains(&(e.deref() as *const _ as *const u8))
    }

    fn buckets<'a>(&'a self) -> &'a [Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>] {
        let slab_front = self.slab_front.load() as usize * size_of::<udeptr>();
        unsafe {
            slice::from_raw_parts(
                self.slab.as_ptr() as *const Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>,
                slab_front / size_of::<Atomic<MarkedEPtr<EBuf>, AtomicUdeptr>>()
            )
        }
    }

    // Memory management
    #[must_use]
    fn alloc_<'a>(&'a self, layout: Layout) -> Result<&'a mut EBuf, Error> {
        // this looks arbitrary, but EBuf should have a pretty reasonable
        // alignment since it contains both function pointers and AtomicUdeptrs
        assert!(layout.align() <= EBuf::ALIGN,
            "unable to alloc alignment {} > {}",
            layout.align(), EBuf::ALIGN
        );

        // find best bucket
        let npw2 = npw2(
            alignup(layout.size(), EBuf::ALIGN)
                / size_of::<udeptr>()
        );

        #[allow(unused_labels)]
        'retry: loop {
            // do we have an allocation in our buckets? we don't look
            // at larger buckets because those are likely to be reused, we don't
            // want to starve larger events with smaller events
            if let Some(bucket) = self.buckets().get(npw2 as usize) {
                // try to take an event from a bucket
                #[cfg(equeue_alloc_mode="lockless")]
                let e = {
                    let eptr = bucket.load();
                    if let Some(e) = eptr.as_ref(self) {
                        if let Err(_) = bucket.cas_marked_inc(eptr, e.sibling.load().eptr) {
                            continue 'retry;
                        }
                        Some(e)
                    } else {
                        None
                    }
                };
                #[cfg(equeue_alloc_mode="locking")]
                let e = {
                    let guard = self.lock.lock();
                    if let Some(e) = bucket.load().as_ref(self) {
                        bucket.store(e.sibling.load());
                        Some(e)
                    } else {
                        None
                    }
                };

                if let Some(e) = e {
                    let mut e = unsafe { e.claim() };

                    // zero certain fields
                    e.cb = None;
                    e.drop = None;
                    e.target = Tick::new(0);
                    e.period = None;
                    e.q = ptr::null();
                    return Ok(e);
                }
            }

            #[cfg(equeue_alloc_mode="lockless")]
            let new_slab_back = {
                // check if we even have enough memory available, we allocate both
                // an event and maybe some buckets if we don't have enough
                let slab_front = self.slab_front.load() as usize * size_of::<udeptr>();
                let slab_back = self.slab_back.load() as usize * size_of::<udeptr>();
                let new_slab_front = max(
                    (npw2 as usize + 1)*size_of::<MarkedEPtr<EBuf>>(),
                    slab_front
                );
                let new_slab_back = slab_back.saturating_sub(
                    size_of::<EBuf>() + (EBuf::ALIGN << npw2)
                );

                if new_slab_front > new_slab_back {
                    return Err(Error::NoMem);
                }

                // commit our allocation starting with the slab_front, worst case
                // if slab_back fails we end up with extra buckets, which isn't the
                // end of the world, the buckets are a sparse array anyways
                if new_slab_front > slab_front {
                    if let Err(_) = self.slab_front.cas(
                        ueptr::try_from(slab_front / size_of::<udeptr>()).unwrap(),
                        ueptr::try_from(new_slab_front / size_of::<udeptr>()).unwrap()
                    ) {
                        continue 'retry;
                    }
                }

                debug_assert!(new_slab_back < slab_back);
                if let Err(_) = self.slab_back.cas(
                    ueptr::try_from(slab_back / size_of::<udeptr>()).unwrap(),
                    ueptr::try_from(new_slab_back / size_of::<udeptr>()).unwrap()
                ) {
                    continue 'retry;
                }

                new_slab_back
            };
            #[cfg(equeue_alloc_mode="locking")]
            let new_slab_back = {
                let guard = self.lock.lock();

                // check if we even have enough memory available, we allocate both
                // an event and maybe some buckets if we don't have enough
                let slab_front = self.slab_front.load() as usize * size_of::<udeptr>();
                let slab_back = self.slab_back.load() as usize * size_of::<udeptr>();
                let new_slab_front = max(
                    (npw2 as usize + 1)*size_of::<MarkedEPtr<EBuf>>(),
                    slab_front
                );
                let new_slab_back = slab_back.saturating_sub(
                    size_of::<EBuf>() + (EBuf::ALIGN << npw2)
                );

                if new_slab_front > new_slab_back {
                    return Err(Error::NoMem);
                }

                // actually commit our allocation
                if new_slab_front > slab_front {
                    self.slab_front.store(
                        ueptr::try_from(new_slab_front / size_of::<udeptr>()).unwrap()
                    );
                }

                debug_assert!(new_slab_back < slab_back);
                self.slab_back.store(
                    ueptr::try_from(new_slab_back / size_of::<udeptr>()).unwrap()
                );

                new_slab_back
            };

            unsafe {
                let e = &self.slab[new_slab_back]
                    as *const u8 as *const EBuf as *mut EBuf;
                e.write(EBuf {
                    next: Atomic::new(MarkedEPtr::null()),
                    next_back: Atomic::new(MarkedEPtr::null()),
                    sibling: Atomic::new(MarkedEPtr::null()),
                    sibling_back: Atomic::new(MarkedEPtr::null()),
                    info: Atomic::new(EInfo::new(0, EState::InFlight, false, false, npw2)),

                    cb: None,
                    drop: None,
                    target: Tick::new(0),
                    period: None,
                    q: ptr::null(),
                });

                return Ok(&mut *e)
            }
        }
    }

    fn dealloc_(&self, e: &mut EBuf) {
        debug_assert!(self.contains(e));

        // make sure to run destructors if assigned, and clear destructors
        // so we don't double-drop if the equeue is itself dropped
        if let Some(drop) = e.drop {
            drop(e.data_mut_ptr());
        }
        e.drop = None;

        // give our event a new id, we don't need to go through the helper
        // fsm for this one because we must already be canceled or exclusively
        // owned at this point
        let info = e.info.load();
        debug_assert_ne!(info.state(), EState::InQueue);
        e.info.store(
            info.inc_id()
                .set_state(EState::InFlight)
                .set_static(false)
                .set_once(false)
        );

        // we can load buckets here because it can never shrink
        let bucket = &self.buckets()[e.npw2() as usize];

        // add our event to a bucket, while also incrementing our
        // generation count
        #[cfg(equeue_alloc_mode="lockless")]
        {
            // push onto bucket
            let mut siblingptr = bucket.load();
            loop {
                debug_assert_ne!(e as *const _, siblingptr.as_ptr(self));
                e.sibling.store_marked(e.sibling.load(), siblingptr.eptr);
                if let Err(siblingptr_) = bucket.cas_marked_inc(
                    siblingptr,
                    e.as_eptr(self)
                ) {
                    siblingptr = siblingptr_;
                    continue;
                }

                break;
            }
        }
        #[cfg(equeue_alloc_mode="locking")]
        {
            let guard = self.lock.lock();

            // push onto bucket
            let siblingptr = bucket.load();
            debug_assert_ne!(e as *const _, siblingptr.as_ptr(self));
            e.sibling.store(siblingptr);
            bucket.store_marked(siblingptr, e.as_eptr(self));
        }
    }

    #[cfg(equeue_queue_mode="lockless")]
    fn help_(&self) -> HelpOp {
        'retry: loop {
            // do we have a help_op to execute?
            let help_op = self.help_op.load();
            if help_op.is_done() {
                return help_op;
            }

            // find help-op location and old value, we store these globally
            // marked by the help-op's generation count to keep things in sync
            let mut help_ctx = self.help_ctx.load();
            let help_atom;
            let help_old;
            if help_ctx.gen != help_op.gen {
                // we must be one behind, if not significant state has changed 
                if help_ctx.gen != help_op.gen.wrapping_sub(1) {
                    continue 'retry;
                }

                // get reference to the atom we are updating
                help_atom = help_op.atom(self);

                // load the old value
                help_old = match help_atom {
                    Some(atom) => atom.load(),
                    None => MarkedEPtr::null(),
                };

                // try to commit the context
                let help_ctx_ = help_ctx
                    .set_mark(help_old.gen)
                    .set_eptr(help_atom.as_eptr(self))
                    .inc();
                if let Err(_) = self.help_ctx.cas(help_ctx, help_ctx_) {
                    continue 'retry;
                }

                help_ctx = help_ctx_;
            } else {
                // get reference to atom we are updating
                help_atom = help_ctx.eptr.as_ref(self);
                
                // we always load the old value from the source
                help_old = match help_atom {
                    Some(atom) => atom.load(),
                    None => MarkedEPtr::null(),
                };
            }

            if let Some(atom) = help_atom
                // if the value does not match help_old, someone _must_ have
                // completed the operation
                .filter(|_| help_old.gen == help_ctx.mark)
            {
                // find new value
                let help_new = help_op.new(self, help_old);

                // perform the actual compare and swap
                if let Err(help_old_) = atom.cas(help_old, help_new) {
                    // oh did someone update the value before us?
                    if help_old_ != help_new {
                        continue 'retry;
                    }
                }
            }

            let help_op_ = help_op.next();
            if let Err(_) = self.help_op.cas(help_op, help_op_) {
                continue 'retry;
            }
        }
    }

    #[cfg(equeue_queue_mode="lockless")]
    fn request_help_<'a>(
        &self,
        help_op: HelpOp,
        state: HelpState,
        e: &'a EBuf,
        estate: EState
    ) -> Result<HelpOp, HelpOp> {
        // help_ should always put help_op into a none state
        debug_assert!(help_op.is_done());

        let help_op_ = help_op
            .set_state(state)
            .set_eptr(e.as_eptr(self))
            .set_estate(estate)
            .inc();
        if let Err(help_op_) = self.help_op.cas(help_op, help_op_) {
            return Err(help_op_);
        }

        Ok(self.help_())
    }

    // Queue management
    #[must_use]
    fn enqueue_<'a>(
        &self,
        e: &'a mut EBuf,
        now: Tick,
        target: Tick
    ) -> Result<bool, &'a mut EBuf> {
        let mut e = e;
        debug_assert!(e.cb.is_some());
        e.target = target;

        'retry: loop {
            let headptr = self.queue.load();
            let queue_mark = headptr.mark;

            // find insertion point
            let mut sibling = None;

            let mut tailptr = headptr;
            let mut tailsrc = &self.queue;
            while let Some(tail) = tailptr.as_ref(self) {
                // compare targets
                match tail.target.cmp(&target) {
                    cmp::Ordering::Greater => {
                        sibling = None;
                        break;
                    }
                    cmp::Ordering::Equal => {
                        sibling = Some(tail);
                        break;
                    }
                    cmp::Ordering::Less => {
                        // continue
                    }
                }

                let nextptr = tail.next.load();

                // check that the previous next pointer hasn't changed on us, if
                // so the node we were traversing could have been removed which
                // means we need to restart
                if tailsrc.load() != tailptr {
                    continue 'retry;
                }

                tailptr = nextptr;
                tailsrc = &tail.next;
            }

            // prepare event before locking
            match sibling {
                None => {
                    // insert a new slice
                    e.next.store_marked(e.next.load(), tailptr.eptr);
                    e.next_back.store_marked(e.next_back.load(), tailsrc.as_eptr(self));
                    e.sibling.store_marked(e.sibling.load(), e.as_eptr(self));
                    e.sibling_back.store_marked(e.sibling_back.load(), e.sibling.as_eptr(self));
                }
                Some(sibling) => {
                    // push onto existing slice
                    e.next.store_marked(e.next.load(), EPtr::null());
                    e.next_back.store_marked(e.next_back.load(), EPtr::null());
                    e.sibling.store_marked(e.sibling.load(), sibling.as_eptr(self));
                }
            }

            // try to insert
            #[cfg(equeue_queue_mode="lockless")]
            loop {
                let help_op = self.help_();

                // are we trying to enqueue an event that's already been canceled?
                //
                // this may seem like a weird place for this check, but it's the
                // only place we lock before re-enqueueing periodic events
                let info = e.info.load();
                match info.state() {
                    EState::Alloced => {},
                    EState::InQueue => break 'retry Ok(false),
                    EState::InFlight => {},
                    EState::Nested => {
                        // nested can only be marked for immediate execuction, but
                        // we can end up here if we are marked nested while trying
                        // to enqueue a periodic/delayed event
                        //
                        // we need to update our target and restart
                        if e.target > now {
                            e.target = now;
                            continue 'retry;
                        }
                    }
                    EState::Canceled => break 'retry Err(e),
                }

                // did someone already change our tailsrc? queue iteration? restart
                if tailsrc.load() != tailptr
                    || self.queue.load().mark != queue_mark
                {
                    continue 'retry;
                }

                // found our insertion point, now lets try to insert
                let change = match sibling {
                    None => {
                        // insert a new slice
                        if let Err(_) = self.request_help_(
                            help_op,
                            HelpState::EnqueueSliceNextBackNext,
                            e,
                            EState::InQueue
                        ) {
                            continue;
                        }

                        tailsrc as *const _ == &self.queue as *const _
                    }
                    Some(sibling) => {
                        // push onto existing slice
                        e.sibling_back.store_marked(e.sibling_back.load(), sibling.sibling_back.load().eptr);

                        if let Err(_) = self.request_help_(
                            help_op,
                            HelpState::EnqueueSiblingSiblingSiblingBack,
                            e,
                            EState::InQueue
                        ) {
                            continue;
                        }

                        false
                    }
                };

                break 'retry Ok(change)
            }
            #[cfg(equeue_queue_mode="locking")]
            {
                let guard = self.lock.lock();

                // are we trying to enqueue an event that's already been canceled?
                //
                // this may seem like a weird place for this check, but it's the
                // only place we lock before re-enqueueing periodic events
                let info = e.info.load();
                match info.state() {
                    EState::Alloced => {},
                    EState::InQueue => break 'retry Ok(false),
                    EState::InFlight => {},
                    EState::Nested => {
                        // nested can only be marked for immediate execuction, but
                        // we can end up here if we are marked nested while trying
                        // to enqueue a periodic/delayed event
                        //
                        // we need to update our target and restart
                        if e.target > now {
                            e.target = now;
                            continue 'retry;
                        }
                    }
                    EState::Canceled => break 'retry Err(e),
                }

                // did someone already change our tailsrc? dequeue iteration? restart
                if tailsrc.load() != tailptr
                    || self.queue.load().mark != queue_mark
                {
                    continue 'retry;
                }

                // found our insertion point, now lets try to insert
                let change = match sibling {
                    None => {
                        // insert a new slice
                        tailsrc.store_marked(tailptr, e.as_eptr(self));
                        if let Some(next) = tailptr.as_ref(self) {
                            next.next_back.store_marked(next.next_back.load(), e.next.as_eptr(self));
                        }

                        tailsrc as *const _ == &self.queue as *const _
                    }
                    Some(sibling) => {
                        // push onto existing slice

                        // the real need for locking here is to make sure all of
                        // the back-references are correct atomically
                        let sibling_backptr = sibling.sibling_back.load();
                        let sibling_back = sibling_backptr.as_ref(self).unwrap();
                        sibling.sibling_back.store_marked(sibling_backptr, e.sibling.as_eptr(self));
                        
                        sibling_back.store_marked(sibling_back.load(), e.as_eptr(self));
                        e.sibling_back.store_marked(e.sibling_back.load(), sibling_back.as_eptr(self));

                        false
                    }
                };

                // mark as pending here, enabling removals
                e.info.store(info.set_state(EState::InQueue));
                break 'retry Ok(change)
            }
        }
    }

    #[must_use]
    fn unqueue_<'a>(
        &'a self,
        e: Either<(&'a EBuf, ugen), ugen>,
        state: EState
    ) -> (bool, Option<&'a mut EBuf>) {
        #[cfg(equeue_queue_mode="lockless")]
        loop {
            let help_op = self.help_();

            // Either unqueue a known event, with specific id, or the head of
            // the dequeue. This looks innocent, but it's important we do both
            // of these checks while locked, since that protects against:
            //
            // 1. Canceling while dispatching
            // 2. Canceling while canceling
            // 3. Multiple dispatchers
            //
            let (e, info) = match e {
                Left((e, id)) => {
                    // still the same event?
                    let info = e.info.load();
                    if info.id != id {
                        return (false, None);
                    }

                    (e, info)
                }
                Right(dequeue_mark) => {
                    // no event specified, pop from dequeue if our gen matches
                    let dequeueptr = self.dequeue.load();
                    let e = match dequeueptr.as_ref(self) {
                        Some(e) if dequeueptr.mark == dequeue_mark => e,
                        _ => return (false, None),
                    };

                    (e, e.info.load())
                }
            };

            // a bit different logic here, we can cancel periodic events, but
            // we can't reclaim the memory if it's in the middle of executing
            match info.state() {
                EState::InQueue => {
                    // we can disentangle the event here and reclaim the memory
                    let nextptr = e.next.load();
                    let next = nextptr.as_ref(self);
                    let next_back = e.next_back.load().as_ref(self);
                    let sibling = e.sibling.load().as_ref(self).unwrap();
                    let sibling_back = e.sibling_back.load().as_ref(self).unwrap();

                    // we also need to remove the queue if we are the head, we can't
                    // just point to queue here with next_back because eptrs are
                    // limited to in-slab events
                    let headptr = self.queue.load();
                    let deheadptr = self.dequeue.load();

                    if next_back.is_some()
                        || headptr.as_ptr(self) == e as *const _
                        || deheadptr.as_ptr(self) == e as *const _
                    {
                        if sibling as *const _ == e as *const _ {
                            // just remove the slice
                            if let Err(_) = self.request_help_(
                                help_op,
                                HelpState::UnqueueSliceNextBackNext,
                                e,
                                state
                            ) {
                                continue;
                            }
                        } else {
                            // remove from siblings
                            if let Err(_) = self.request_help_(
                                help_op,
                                HelpState::UnqueueSiblingSiblingNext,
                                e,
                                state
                            ) {
                                continue;
                            }
                        }
                    } else {
                        // no next_back just means we are a sibling, we still need
                        // to remove from siblings
                        if let Err(_) = self.request_help_(
                            help_op,
                            HelpState::UnqueueSiblingBackSibling,
                            e,
                            state
                        ) {
                            continue;
                        }
                    }

                    // note we are responsible for the memory now
                    break (true, Some(unsafe { e.claim() }))
                }
                EState::Alloced => {
                    // alloced is a weird one, if we end up here, we just need
                    // to claim the event, and since we ensure no ids coexist
                    // mutable references to events at the type-level, we can
                    // be sure we have exclusive access
                    if let Err(_) = self.request_help_(
                        help_op,
                        HelpState::UpdateState,
                        e,
                        state
                    ) {
                        continue;
                    }

                    break (true, Some(unsafe { e.claim() }))
                }
                EState::InFlight | EState::Nested => {
                    // if we're periodic/static and currently executing best we
                    // can do is mark the event so it isn't re-enqueued
                    if let Err(_) = self.request_help_(
                        help_op,
                        HelpState::UpdateState,
                        e,
                        state
                    ) {
                        continue;
                    }

                    break (info.static_(), None)
                }
                EState::Canceled => {
                    break (false, None)
                }
            }
        }
        #[cfg(equeue_queue_mode="locking")]
        {   let guard = self.lock.lock();

            // Either unqueue a known event, with specific id, or the head of
            // the dequeue. This looks innocent, but it's important we do both
            // of these checks while locked, since that protects against:
            //
            // 1. Canceling while dispatching
            // 2. Canceling while canceling
            // 3. Multiple dispatchers
            //
            let (e, info) = match e {
                Left((e, id)) => {
                    // still the same event?
                    let info = e.info.load();
                    if info.id != id {
                        return (false, None);
                    }

                    (e, info)
                }
                Right(dequeue_mark) => {
                    // no event specified, pop from dequeue if our gen matches
                    let dequeueptr = self.dequeue.load();
                    let e = match dequeueptr.as_ref(self) {
                        Some(e) if dequeueptr.mark == dequeue_mark => e,
                        _ => return (false, None),
                    };

                    (e, e.info.load())
                }
            };

            // a bit different logic here, we can cancel periodic events, but
            // we can't reclaim the memory if it's in the middle of executing
            match info.state() {
                EState::InQueue => {
                    // we can disentangle the event here and reclaim the memory
                    let nextptr = e.next.load();
                    let next = nextptr.as_ref(self);
                    let next_back = e.next_back.load().as_ref(self);
                    let sibling = e.sibling.load().as_ref(self).unwrap();
                    let sibling_back = e.sibling_back.load().as_ref(self).unwrap();

                    // we also need to remove the queue if we are the head, we can't
                    // just point to queue here with next_back because eptrs are
                    // limited to in-slab events
                    let headptr = self.queue.load();
                    let deheadptr = self.dequeue.load();

                    if next_back.is_some()
                        || headptr.as_ptr(self) == e as *const _
                        || deheadptr.as_ptr(self) == e as *const _
                    {
                        if sibling as *const _ == e as *const _ {
                            // just remove the slice

                            // update next_back's next/queue head first to avoid invalidating traversals
                            if let Some(next_back) = next_back {
                                next_back.store_marked(next_back.load(), nextptr.eptr);
                            }
                            if let Some(next) = next {
                                next.next_back.store_marked(next.next_back.load(), next_back.as_eptr(self));
                            }
                        } else {
                            // remove from siblings

                            // we need this claim here to notate that it's safe to create
                            // a markedptr from sibling, this loads the generation count from
                            // the sibling's next pointer, which could be a race condition
                            // if we aren't locked
                            let sibling = unsafe { sibling.claim() };
                            sibling.next.store_marked(sibling.next.load(), nextptr.eptr);
                            sibling.next_back.store_marked(sibling.next_back.load(), next_back.as_eptr(self));

                            // update next_back's next/queue head first to avoid invalidating traversals
                            if let Some(next_back) = next_back {
                                next_back.store_marked(next_back.load(), sibling.as_eptr(self));
                            }
                            if let Some(next) = next {
                                next.next_back.store_marked(next.next_back.load(), sibling.next.as_eptr(self));
                            }
                        }
                    }

                    sibling_back.store_marked(sibling_back.load(), sibling.as_eptr(self));
                    sibling.sibling_back.store_marked(sibling.sibling_back.load(), sibling_back.as_eptr(self));

                    // mark as removed
                    e.next.store_marked_inc(nextptr, EPtr::null());
                    // mark as not-pending
                    e.info.store(e.info.load().set_state(state));
                    
                    // note we are responsible for the memory now
                    (true, Some(unsafe { e.claim() }))
                }
                EState::Alloced => {
                    // alloced is a weird one, if we end up here, we just need
                    // to claim the event, and since we ensure no ids coexist
                    // mutable references to events at the type-level, we can
                    // be sure we have exclusive access
                    e.info.store(e.info.load().set_state(state));
                    (true, Some(unsafe { e.claim() }))
                }
                EState::InFlight | EState::Nested => {
                    // if we're periodic/static and currently executing best we
                    // can do is mark the event so it isn't re-enqueued
                    e.info.store(e.info.load().set_state(state));
                    (info.static_(), None)
                }
                EState::Canceled => {
                    (false, None)
                }
            }
        }
    }

    #[must_use]
    fn dequeue_<'a>(
        &'a self,
        now: Tick
    ) -> impl Iterator<Item=&'a mut EBuf> + 'a {
        let dequeue_mark = 'retry: loop {
            // dispatch already in progress? let's try to help out
            let deheadptr = self.dequeue.load();
            if deheadptr.as_ref(self).is_some() {
                break 'retry deheadptr.mark;
            }

            // find all events ready to execute
            let headptr = self.queue.load();
            let mut back = None;

            let mut tailptr = headptr;
            let mut tailsrc = &self.queue;
            while let Some(tail) = tailptr.as_ref(self) {
                // not ready?
                if tail.target > now {
                    break;
                }

                back = Some(tail);
                let nextptr = tail.next.load();

                // check that the previous next pointer hasn't changed on us, if
                // so the node we were traversing could have been removed which
                // means we need to restart
                if tailsrc.load() != tailptr {
                    continue 'retry;
                }

                tailptr = nextptr;
                tailsrc = &tail.next;
            }

            // no events ready?
            if tailsrc as *const _ == &self.queue as *const _ {
                return Left(iter::empty());
            }

            // try to unroll events
            #[cfg(equeue_queue_mode="lockless")]
            loop {
                let help_op = self.help_();

                // did someone already change our tailsrc? dequeue? queue? restart
                if tailsrc.load() != tailptr
                    || self.dequeue.load() != deheadptr
                    || self.queue.load() != headptr
                {
                    continue 'retry;
                }

                // cut our unrolled queue
                if let Err(_) = self.request_help_(
                    help_op,
                    HelpState::DequeueDequeue,
                    back.unwrap(),
                    EState::InQueue,
                ) {
                    continue;
                }

                break 'retry deheadptr.mark.wrapping_add(1);
            }
            #[cfg(equeue_queue_mode="locking")]
            {   let guard = self.lock.lock();

                // did someone already change our tailsrc? dequeue? queue? restart
                if tailsrc.load() != tailptr
                    || self.dequeue.load() != deheadptr
                    || self.queue.load() != headptr
                {
                    continue 'retry;
                }

                // point dequeue to the head of ready events
                self.dequeue.store_marked(
                    deheadptr.set_mark(deheadptr.mark.wrapping_add(1)),
                    headptr.eptr
                );
                // point queue to the tail of ready events
                self.queue.store_marked(
                    headptr.set_mark(headptr.mark.wrapping_add(1)),
                    tailptr.eptr
                );

                // cut our unrolled queue
                let head = headptr.as_ref(self).unwrap();
                head.next_back.store_marked(head.next_back.load(), self.dequeue.as_eptr(self));
                if let Some(tail) = tailptr.as_ref(self) {
                    tail.next_back.store_marked(tail.next_back.load(), self.queue.as_eptr(self));
                }
                back.unwrap().next.store_marked_inc(tailptr, EPtr::null());

                break 'retry deheadptr.mark.wrapping_add(1);
            }
        };

        // unqueue from the dequeue list an event at a time
        Right(iter::from_fn(move || {
            self.unqueue_(Right(dequeue_mark), EState::InFlight).1
        }))
    }

    fn update_state_<F>(&self, e: &EBuf, mut f: F) -> EInfo
    where
        F: FnMut(EInfo) -> Option<EState>
    {
        #[cfg(equeue_queue_mode="lockless")]
        loop {
            let help_op = self.help_();

            let info = e.info.load();
            if let Some(state_) = f(info) {
                if let Err(_) = self.request_help_(
                    help_op,
                    HelpState::UpdateState,
                    e,
                    state_
                ) {
                    continue;
                }
            }

            break info;
        }
        #[cfg(equeue_queue_mode="locking")]
        {
            let guard = self.lock.lock();

            let info = e.info.load();
            if let Some(state_) = f(info) {
                e.info.store(info.set_state(state_));
            }
            info
        }
    }

    fn update_break_<F>(&self, mut f: F) -> u8
    where
        F: FnMut(u8) -> Option<u8>
    {
        #[cfg(equeue_break_mode="lockless")]
        {
            let mut break_ = self.break_.load();
            loop {
                if let Some(break__) = f(break_) {
                    if let Err(break__) = self.break_.cas(break_, break__) {
                        break_ = break__;
                        continue;
                    }
                }

                return break_;
            }
        }
        #[cfg(equeue_break_mode="locking")]
        {
            let guard = self.lock.lock();

            let break_ = self.break_.load();
            if let Some(break__) = f(break_) {
                self.break_.store(break__);
            }
            break_
        }
    }
}

impl<C: Clock> Equeue<C> {
    fn now(&self) -> Tick {
        Tick::new(self.clock.now())
    }

    // How long until event executes?
    fn delta_id_(&self, id: Id) -> Option<Delta> {
        let e = match id.as_ref(self) {
            Some(e) => e,
            None => return None,
        };

        // load target/period first so we can be sure these
        // don't change after we read id
        let target = e.target;
        let period = e.period;
        let info = e.info.load();
        if info.id != id.id {
            return None;
        }

        // note that periodic events are always pending, we load period
        // first since it is not necessarily atomic
        match info.state() {
            EState::Alloced  => None,
            EState::InQueue  => Some(target - self.now()),
            EState::InFlight => period,
            EState::Nested   => Some(Delta::zero()),
            EState::Canceled => None,
        }
    }

    pub fn delta_id<??: TryFromDelta>(&self, id: Id) -> Option<??> {
        self.delta_id_(id)
            .map(|delta|
                ??::try_from_delta(delta, self.clock.frequency()).ok()
                    .expect("equeue: delta overflow")
            )
    }
}

impl<C> Equeue<C> {
    // Central cancel function
    pub fn cancel(&self, id: Id) -> bool {
        // check to see if we need to bother locking
        let e = match id.as_ref(self) {
            Some(e) => e,
            None => return false,
        };

        let info = e.info.load();
        if info.id != id.id || info.state() == EState::Canceled {
            return false;
        }

        let (canceled, e) = self.unqueue_(Left((e, id.id)), EState::Canceled);

        if let Some(e) = e {
            // make sure to clean up memory
            self.dealloc_(e);
        }

        canceled
    }
}

impl<C: Clock+Signal> Equeue<C> {
    // Central post function
    fn post_(&self, e: &mut EBuf, delta: Delta) {
        // calculate target
        let now = self.now();
        let target = now.imprecise_add(delta, self.precision);

        // all periodic events are static events
        if e.period.is_some() {
            e.info.store(e.info.load().set_static(true));
        }

        let delta_changed = self.enqueue_(e, now, target).unwrap();

        // signal queue has changed
        if delta_changed {
            self.clock.signal();
        }
    }

    // post an static event which may already be pending
    pub fn post_id(&self, id: Id) -> bool {
        let e = match id.as_ref(self) {
            Some(e) => e,
            None => return false,
        };

        // still the same event?
        if e.info.load().id != id.id {
            return false;
        }

        loop {
            // The main difference between post and post is that we may
            // be pending an event that's already pending. This means several
            // more corner cases to handle.
            let now = self.now();
            let info = self.update_state_(e, |info| {
                match info.state() {
                    // still the same event?
                    _ if info.id != id.id              => None,
                    EState::Alloced                    => Some(EState::InFlight),
                    EState::InFlight if info.static_() => Some(EState::Nested),
                    _                                  => None,
                }
            });

            match info.state() {
                // still the same event?
                _ if info.id != id.id => {
                    return false;
                }
                EState::Alloced => {
                    // if we're alloced we can just enqueue, make sure to mark
                    // and claim the event first
                    let e = unsafe { e.claim() };
                    match self.enqueue_(e, now, now) {
                        Ok(delta_changed) => {
                            // signal queue has changed
                            if delta_changed {
                                self.clock.signal();
                            }
                            return true;
                        }
                        Err(e) => {
                            // surprisingly enough, this can fail here,
                            // if we're canceled as we enqueue
                            self.dealloc_(e);
                            return false;
                        }
                    }
                }
                EState::InQueue if now < e.target => {
                    // we're pending, but in the future, we want to move this
                    // to execute immediately, try to unqueue and continue the
                    // loop to reenqueue
                    let _ = self.unqueue_(Left((e, id.id)), EState::InFlight);
                    continue;
                }
                EState::InFlight if info.static_() => {
                    // someone else is dispatching, just make sure we mark that
                    // we are interested in the event being repended
                    return true;
                }
                EState::InQueue | EState::Nested => {
                    // do nothing, the event is already pending
                    return true;
                }
                EState::InFlight | EState::Canceled => {
                    // do nothing, the event is canceled
                    return false;
                }
            }
        }
    }
}

impl<C: Clock> Equeue<C> {
    // find time until next event without locking
    //
    // note this is clamped to 0 at minimum
    fn delta_(&self, now: Tick) -> Option<Delta> {
        'retry: loop {
            // wait, if break is requested we need to process
            // it immediately
            if self.break_.load() > 0 {
                return Some(Delta::zero());
            }

            let headptr = self.queue.load();
            let target = match headptr.as_ref(self) {
                Some(head) => head.target,
                None => return None,
            };

            // make sure headptr hasn't changed before we trust our target
            if self.queue.load() != headptr {
                continue 'retry;
            }

            break Some(target - now);
        }
    }

    pub fn delta<??: TryFromDelta>(&self) -> Option<??> {
        self.delta_(self.now())
            .map(|delta|
                ??::try_from_delta(delta, self.clock.frequency()).ok()
                    .expect("equeue: delta overflow")
            )
    }

    // Dispatch ready events
    pub fn dispatch_ready(&self) {
        // get the current time
        let now = self.now();

        // get a slice to dispatch
        for e in self.dequeue_(now) {
            // load id here so we can tell if it changes (which happens if the
            // event is reclaimed in dispatch)
            let id = e.info.load().id;

            // dispatch!
            (e.cb.unwrap())(e.data_mut_ptr());

            let info = e.info.load();
            let e = if id != info.id {
                // deallocated while dispatching?
                None
            } else if let Some(period) = e.period {
                // if periodic, reenqueue, unless we get canceled
                let now = self.now();
                self.enqueue_(e, now, now.imprecise_add(
                    period,
                    self.precision
                )).err()
            } else {
                // try to mark as no-longer in-queue, but note we could be
                // canceled or recursively pended if we are static
                let info = self.update_state_(e, |info| {
                    match (info.state(), info.static_()) {
                        (EState::InFlight, true)  => Some(EState::Alloced),
                        (EState::InFlight, false) => Some(EState::Canceled),
                        (EState::Nested,   true)  => None,
                        (EState::Canceled, _)     => None,
                        _ => unreachable!(),
                    }
                });
                match (info.state(), info.static_()) {
                    (EState::InFlight, true)  => None,
                    (EState::InFlight, false) => Some(e),
                    (EState::Nested,   true)  => {
                        let now = self.now();
                        self.enqueue_(e, now, now).err()
                    }
                    (EState::Canceled, _)     => Some(e),
                    _ => unreachable!(),
                }
            };

            // release memory?
            if let Some(e) = e { 
                // call drop, return event to memory pool
                self.dealloc_(e);
            }
        }
    }
}

impl<C: Clock+Sema> Equeue<C> {
    // Central dispatch function
    fn dispatch_(&self, delta: Option<Delta>) -> Dispatch {
        // calculate the timeout
        let mut now = self.now();
        let timeout = delta.map(|delta| now + delta);

        loop {
            // dispatch events
            self.dispatch_ready();

            // was break requested?
            if self.update_break_(|break_| break_.checked_sub(1)) > 0 {
                return Dispatch::Break;
            }

            // should we stop dispatching?
            //
            // note that time could have changed _significantly_
            now = self.now();
            let timeout_left = timeout.map(|timeout| timeout - now);
            if let Some(timeout_left) = timeout_left {
                if timeout_left <= Delta::zero() {
                    return Dispatch::Timeout;
                }
            }

            // ok how long should we sleep for
            //
            // Note that we always try to sleep between slices, this is
            // just to behave nicely in case the system's semaphore implementation
            // does something "clever". Note we also never enter here if
            // ticks is 0 for similar reasons.
            let delta = self.delta_(now)
                .into_iter().chain(timeout_left)
                .min();

            match delta {
                Some(delta) => self.clock.wait_timeout(delta),
                None => self.clock.wait(),
            }
        }
    }

    #[inline]
    pub fn dispatch_for<??: TryIntoDelta>(&self, delta: ??) -> Dispatch {
        self.dispatch_(Some(
            delta.try_into_delta(self.clock.frequency()).ok()
                .expect("equeue: delta overflow")
        ))
    }

    #[inline]
    pub fn dispatch(&self) -> Dispatch {
        self.dispatch_(None)
    }
}

impl<C: Signal> Equeue<C> {
    // request dispatch to exit once done dispatching events
    pub fn break_(&self) {
        self.update_break_(|break_| Some(break_ + 1));
        self.clock.signal();
    }
}

// Raw allocations
impl<C> Equeue<C> {
    pub unsafe fn alloc_raw(
        &self,
        layout: Layout,
        cb: fn(*mut u8),
        drop: fn(*mut u8)
    ) -> *mut u8 {
        let e = match self.alloc_(layout) {
            Ok(e) => e,
            Err(_) => {
                return ptr::null_mut();
            }
        };

        e.cb = Some(cb);
        e.drop = Some(drop);

        e.data_mut_ptr()
    }

    pub unsafe fn dealloc_raw(&self, e: *mut u8) {
        let e = EBuf::from_data_mut_ptr(e).unwrap();
        debug_assert!(self.contains(e));
        self.dealloc_(e);
    }
}


/// An id we can use to try to cancel an event
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Id {
    id: ugen,
    eptr: EPtr<EBuf>,
}

impl Id {
    pub const fn null() -> Id {
        // Ids can not be zero because that's where our buckets go in the slab
        Id {
            id: 0,
            eptr: EPtr::null(),
        }
    }

    fn new<C>(q: &Equeue<C>, id: ugen, e: &mut EBuf) -> Id {
        Id {
            id: id,
            eptr: e.as_eptr(q),
        }
    }

    fn as_ref<'a, C>(&self, q: &'a Equeue<C>) -> Option<&'a EBuf> {
        self.eptr.as_ref(q)
    }
}

impl Default for Id {
    fn default() -> Id {
        Id::null()
    }
}


/// A strongly-typed alternative to Id which implicitly cancels the
/// event when dropped
#[derive(Debug)]
pub struct Handle<
    'a,
    #[cfg(feature="std")] C=SysClock,
    #[cfg(not(feature="std"))] C,
> {
    q: &'a Equeue<C>,
    id: Id,
}

impl<'a, C> Handle<'a, C> {
    fn new(q: &'a Equeue<C>, id: Id) -> Handle<'a, C> {
        Handle {
            q: q,
            id: id,
        }
    }

    pub fn id(&self) -> Id {
        self.id
    }

    // Explicitly cancel the event
    pub fn cancel(&self) -> bool {
        self.q.cancel(self.id)
    }
}

impl<'a, C: Clock> Handle<'a, C> {
    // Some other convenience functions, which can
    // normally be done with Ids
    pub fn delta<??: TryFromDelta>(&self) -> Option<??> {
        self.q.delta_id(self.id)
    }
}

impl<'a, C: Clock+Signal> Handle<'a, C> {
    pub fn post(&self) -> bool {
        self.q.post_id(self.id)
    }
}

impl<C> Drop for Handle<'_, C> {
    fn drop(&mut self) {
        self.q.cancel(self.id);
    }
}


// event waker vtable callbacks
unsafe fn event_waker_clone<C: Clock+Signal>(e: *const ()) -> RawWaker {
    RawWaker::new(e, &Equeue::<C>::EVENT_WAKER_VTABLE)
}

unsafe fn event_waker_wake<C: Clock+Signal>(e: *const ()) {
    // Fitting event ids into RawWaker is a bit frustrating
    //
    // Because of equeue's history, we already provide a unique id for each
    // event built out of a compressed pointer and counter that fits into
    // a single word. However, we also need to know which actual equeue
    // instance we're running on. Waker only has a single word of storage,
    // so this sort of ruins our plans (we really can't use an Arc here
    // because this is intended for no-alloc systems).
    //
    // We want:
    // [--   q   --][- id -|-  e -]
    //       ^          ^      ^-- compressed eptr
    //       |          '--------- id of event
    //       '-------------------- pointer to equeue
    // 
    // In order to make this work with the RawWaker interface:
    //
    // 1. We store a reference to the equeue instance in the event, this
    //    costs an extra word of space for each event which is otherwise
    //    unused
    //
    // 2. We take advantage of alignment to store a truncated id in the lower
    //    bits of the pointer. Whether or not this is enough to prevent bad id
    //    matches in practice is unknown.
    //
    // [--   e   -|id]
    //       ^      ^-- truncated id of event
    //       '--------- uncompressed pointer to event

    let mask = EBuf::ALIGN-1;
    let id_trunc = ((e as usize) & mask) as ugen;
    let e = EBuf::from_data_mut_ptr(
        ((e as usize) & !mask) as *mut ()
    ).unwrap();

    // check that id matches first
    let id = e.info.load().id;
    if id_trunc != id & mask as ugen {
        return;
    }

    // post
    let q = (e.q as *const Equeue<C>).as_ref().unwrap();
    q.post_id(Id::new(q, id, e));
}

unsafe fn event_waker_drop(e: *const ()) {
    // do nothing
}

impl<C: Clock+Signal> Equeue<C> {
    const EVENT_WAKER_VTABLE: RawWakerVTable = RawWakerVTable::new(
        event_waker_clone::<C>,
        event_waker_wake::<C>,
        event_waker_wake::<C>,
        event_waker_drop,
    );
}


// the issue is that we'd need to store a fat pointer somehow
/// Event handle
#[derive(Debug)]
pub struct Event<
    'a,
    T: ?Sized,
    #[cfg(feature="std")] C=SysClock,
    #[cfg(not(feature="std"))] C,
> {
    q: &'a Equeue<C>,
    t: &'a mut T,
}

impl<'a, T, C> Event<'a, T, C> {
    fn new(q: &'a Equeue<C>, e: &'a mut EBuf) -> Event<'a, T, C> {
        Event {
            q: q,
            t: unsafe { e.data_mut() },
        }
    }
}

impl<'a, T: ?Sized, C> Event<'a, T, C> {
    fn ebuf<'b>(&'b mut self) -> &'b mut EBuf {
        unsafe { EBuf::from_data_mut(self.t) }
    }
}

impl<'a, T: ?Sized, C> Event<'a, T, C> {
    pub fn into_raw(self) -> (&'a Equeue<C>, *mut T) {
        let raw = (self.q, self.t as *mut T);
        forget(self);
        raw
    }

    pub unsafe fn from_raw(q: &'a Equeue<C>, t: *mut T) -> Event<'a, T, C> {
        Event {
            q: q,
            t: &mut *t,
        }
    }
}

impl<C> Equeue<C> {
    pub fn alloc<'a, T: Post + Send>(&'a self, t: T) -> Result<Event<'a, T, C>, Error> {
        let e = self.alloc_(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(t); }

        // cb/drop thunks
        fn cb_thunk<T: Post>(e: *mut u8) {
            unsafe { &mut *(e as *mut T) }.post();
        }

        fn drop_thunk<T>(e: *mut u8) {
            unsafe { drop_in_place(e as *mut T) };
        }

        e.cb = Some(cb_thunk::<T>);
        e.drop = Some(drop_thunk::<T>);

        Ok(Event::new(self, e))
    }

    pub fn alloc_once<'a, T: PostOnce + Send>(&'a self, t: T) -> Result<Event<'a, T, C>, Error> {
        let e = self.alloc_(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(t); }

        // cb/drop thunks
        fn cb_thunk<T: PostOnce>(e: *mut u8) {
            // because we're post once, posting implicitly drops the callback,
            // however we still want to drop the callback correctly if we're
            // explicitly dropped and never called, we manage this by clearing
            // our destructor when called
            let t = unsafe { (e as *mut T).read()};
            unsafe { EBuf::from_data_mut_ptr(e) }.unwrap().drop = None;
            t.post_once();
        }

        fn drop_thunk<T: PostOnce>(e: *mut u8) {
            unsafe { drop_in_place(e as *mut T) };
        }

        e.cb = Some(cb_thunk::<T>);
        e.drop = Some(drop_thunk::<T>);

        // mark as a one-time event
        e.info.store(e.info.load().set_once(true));
        Ok(Event::new(self, e))
    }

    pub fn alloc_static<'a, T: PostStatic<Event<'a, T, C>> + Send>(&'a self, t: T) -> Result<Event<'a, T, C>, Error> {
        let e = self.alloc_(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(t); }
        e.q = self as *const Equeue<C> as *const Equeue<()>;

        // cb/drop thunks
        fn cb_thunk<'a, T: PostStatic<Event<'a, T, C>> + 'a, C: 'a>(e: *mut u8) {
            let e = unsafe { EBuf::from_data_mut_ptr(e) }.unwrap();
            let e = Event::new(unsafe { (e.q as *const Equeue<C>).as_ref() }.unwrap(), e);
            T::post_static(e);
        }

        fn drop_thunk<T>(e: *mut u8) {
            unsafe { drop_in_place(e as *mut T) };
        }

        e.cb = Some(cb_thunk::<T, C>);
        e.drop = Some(drop_thunk::<T>);

        // mark as static
        e.info.store(e.info.load().set_static(true));
        Ok(Event::new(self, e))
    }
}

impl<C: Clock+Signal> Equeue<C> {
    pub fn alloc_future<'a, T: Future<Output=()> + Send>(&'a self, t: T) -> Result<Event<'a, T, C>, Error> {
        let e = self.alloc_(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(t); }
        e.q = self as *const Equeue<C> as *const Equeue<()>;

        fn cb_thunk<T: Future<Output=()>, C: Clock+Signal>(e: *mut u8) {
            let e = unsafe { EBuf::from_data_mut_ptr(e) }.unwrap();
            debug_assert!(e.info.load().static_());
            let mut e = Event::<T, C>::new(unsafe { (e.q as *const Equeue<C>).as_ref() }.unwrap(), e);

            // setup waker
            let waker = unsafe {
                Waker::from_raw(event_waker_clone::<C>(
                    e.deref() as *const T as *const ()
                ))
            };
            let mut context = Context::from_waker(&waker);
            let pinned = unsafe { Pin::new_unchecked(e.deref_mut()) };

            match pinned.poll(&mut context) {
                Poll::Pending => {
                    // not done, forget so we don't clean up
                    forget(e);
                },
                Poll::Ready(()) => {
                    // done! dropping e will automatically clean up the memory
                }
            }
        }

        fn drop_thunk<T>(e: *mut u8) {
            unsafe { drop_in_place(e as *mut T) };
        }

        e.cb = Some(cb_thunk::<T, C>);
        e.drop = Some(drop_thunk::<T>);

        // mark as static
        e.info.store(e.info.load().set_static(true));
        Ok(Event::new(self, e))
    }
}

impl<'a, T: ?Sized, C: Clock+Signal> Event<'a, T, C> {
    pub fn delay<??: TryIntoDelta>(mut self, delay: ??) -> Self {
        self.ebuf().target = delay.try_into_delta(self.q.clock.frequency()).ok()
            .expect("equeue: delta overflow")
            .as_tick();
        self
    }

    pub fn period<??: TryIntoDelta>(mut self, period: Option<??>) -> Self {
        // can't set period for PostOnce events 
        assert!(!self.ebuf().info.load().once());
        self.ebuf().period = period.map(|period|
            period.try_into_delta(self.q.clock.frequency()).ok()
                .expect("equeue: delta overflow")
        );
        self
    }

    pub fn static_(mut self, static_: bool) -> Self {
        // can't make PostOnce events static
        assert!(!self.ebuf().info.load().once());
        let e = self.ebuf();
        e.info.store(e.info.load().set_static(static_));
        self
    }

    // note this consumes the Event, otherwise we'd risk multiple access
    // if the id is pended while we still have a mutable reference
    pub fn into_id(mut self) -> Id {
        let q = self.q;
        let e = self.ebuf();

        // mark as no longer in use, allowing external pends
        let info = e.info.load();
        e.info.store(info.set_state(EState::Alloced));

        let id = Id::new(q, info.id, e);
        forget(self);
        id
    }

    pub fn into_handle(self) -> Handle<'a, C> {
        Handle::new(self.q, self.into_id())
    }

    pub fn post(mut self) -> Id {
        // enqueue and then forget the event, it's up to equeue to
        // drop the event later
        let q = self.q;
        let e = self.ebuf();

        let id = Id::new(q, e.info.load().id, e);
        q.post_(e, e.target.as_delta().unwrap());
        forget(self);
        id
    }

    pub fn post_handle(self) -> Handle<'a, C> {
        Handle::new(self.q, self.post())
    }
}

impl<T: ?Sized, C> Drop for Event<'_, T, C> {
    fn drop(&mut self) {
        // clean up ebuf, note this runs the destructor internally
        self.q.dealloc_(self.ebuf());
    }
}

impl<T: ?Sized, C> Deref for Event<'_, T, C> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.t
    }
}

impl<T: ?Sized, C> DerefMut for Event<'_, T, C> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.t
    }
}

impl<T: ?Sized, C> AsRef<T> for Event<'_, T, C> {
    fn as_ref(&self) -> &T {
        &self.t
    }
}

impl<T: ?Sized, C> AsMut<T> for Event<'_, T, C> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.t
    }
}


// convenience functions
impl<C: Clock+Signal> Equeue<C> {
    pub fn call<F: PostOnce + Send>(
        &self,
        cb: F
    ) -> Result<Id, Error> {
        Ok(
            self.alloc_once(cb)?
                .post()
        )
    }

    pub fn call_in<??: TryIntoDelta, F: PostOnce + Send>(
        &self,
        delay: ??,
        cb: F
    ) -> Result<Id, Error> {
        Ok(
            self.alloc_once(cb)?
                .delay(delay)
                .post()
        )
    }

    pub fn call_every<??: TryIntoDelta, F: Post + Send>(
        &self,
        period: ??,
        cb: F
    ) -> Result<Id, Error> {
        let period = period.try_into_delta(self.clock.frequency()).ok()
            .expect("equeue: delta overflow");
        Ok(
            self.alloc(cb)?
                .delay(period)
                .period(Some(period))
                .post()
        )
    }

    pub fn run<F: Future<Output=()> + Send>(
        &self,
        cb: F
    ) -> Result<Id, Error> {
        Ok(
            self.alloc_future(cb)?
                .post()
        )
    }

    pub fn call_handle<'a, F: PostOnce + Send>(
        &'a self,
        cb: F
    ) -> Result<Handle<'a, C>, Error> {
        self.call(cb).map(|id| Handle::new(self, id))
    }

    pub fn call_in_handle<'a, ??: TryIntoDelta, F: PostOnce + Send>(
        &'a self,
        delay: ??,
        cb: F
    ) -> Result<Handle<'a, C>, Error> {
        self.call_in(delay, cb).map(|id| Handle::new(self, id))
    }

    pub fn call_every_handle<'a, ??: TryIntoDelta, F: Post + Send>(
        &'a self,
        period: ??,
        cb: F
    ) -> Result<Handle<'a, C>, Error> {
        self.call_every(period, cb).map(|id| Handle::new(self, id))
    }

    pub fn run_handle<'a, F: Future<Output=()> + Send>(
        &'a self,
        cb: F
    ) -> Result<Handle<'a, C>, Error> {
        self.run(cb).map(|id| Handle::new(self, id))
    }
}


// async functions

// NOTE, these could actually be optimized to not require additional allocations
// for timeouts, however we would need to know if we are currently executing in
// an Equeue instance, and have some way to access the Equeue.
//
// This is currently not possible. We could get to the Equeue from the RawWaker
// instance, but the Context/Waker wrappers are a one-way type that can't be
// reversed safely.
//
// An alternative would be storing additional data in the Context (why else
// does this class exist), but this is also not supported.
//
// https://github.com/rust-lang/rfcs/issues/2900

#[derive(Debug)]
struct AsyncYield<
    'a,
    #[cfg(feature="std")] C=SysClock,
    #[cfg(not(feature="std"))] C,
> {
    q: &'a Equeue<C>,
    yielded: bool
}

impl<C> Future for AsyncYield<'_, C> {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // just pend once
        if self.yielded {
            Poll::Ready(())
        } else {
            cx.waker().wake_by_ref();
            self.yielded = true;
            Poll::Pending
        }
    }
}

#[derive(Debug)]
struct AsyncSleep<
    'a,
    #[cfg(feature="std")] C=SysClock,
    #[cfg(not(feature="std"))] C,
> {
    q: &'a Equeue<C>,
    yielded: bool,
    timeout: Tick,
    id: Id,
}

impl<C: Clock+Signal> Future for AsyncSleep<'_, C> {
    type Output = Result<(), Error>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // expired?
        let delta = self.timeout - self.q.now();
        if delta <= Delta::zero() {
            if self.yielded {
                Poll::Ready(Ok(()))
            } else {
                // make sure we always yield at least once
                cx.waker().wake_by_ref();
                self.yielded = true;
                Poll::Pending
            }
        } else {
            self.q.cancel(self.id);
            let waker = cx.waker().clone();
            match self.q.call_in(delta, move || waker.wake()) {
                Ok(id) => {
                    self.id = id;
                    self.yielded = true;
                    Poll::Pending
                }
                Err(err) => {
                    // oops, ran out of memory
                    Poll::Ready(Err(err))
                }
            }
        }
    }
}

impl<C> Drop for AsyncSleep<'_, C> {
    fn drop(&mut self) {
        // make sure we cancel our timeout
        self.q.cancel(self.id);
    }
}

#[derive(Debug)]
struct AsyncTimeout<
    'a,
    F,
    #[cfg(feature="std")] C=SysClock,
    #[cfg(not(feature="std"))] C,
> {
    q: &'a Equeue<C>,
    f: F,
    timeout: Tick,
    id: Id,
}

impl<F: Future, C: Clock+Signal> Future for AsyncTimeout<'_, F, C> {
    type Output = Result<F::Output, Error>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let self_ = unsafe { self.get_unchecked_mut() };

        // first, is f ready?
        let f = unsafe { Pin::new_unchecked(&mut self_.f) };
        if let Poll::Ready(r) = f.poll(cx) {
            self_.q.cancel(self_.id);
            return Poll::Ready(Ok(r));
        }

        // expired?
        let delta = self_.timeout - self_.q.now();
        if delta <= Delta::zero() {
            Poll::Ready(Err(Error::Timeout))
        } else {
            self_.q.cancel(self_.id);
            let waker = cx.waker().clone();
            match self_.q.call_in(delta, move || waker.wake()) {
                Ok(id) => {
                    self_.id = id;
                    Poll::Pending
                }
                Err(err) => {
                    // oops, ran out of memory
                    Poll::Ready(Err(err))
                }
            }
        }
    }
}

impl<F, C> Drop for AsyncTimeout<'_, F, C> {
    fn drop(&mut self) {
        // make sure we cancel our timeout
        self.q.cancel(self.id);
    }
}

impl<C: Clock+Signal> Equeue<C> {
    pub async fn yield_(&self) {
        AsyncYield {
            q: self,
            yielded: false
        }.await
    }

    pub async fn sleep<??: TryIntoDelta>(&self, delta: ??) -> Result<(), Error> {
        let delta = delta.try_into_delta(self.clock.frequency()).ok()
            .expect("equeue: delta overflow");
        AsyncSleep {
            q: self,
            yielded: false,
            timeout: self.now() + delta,
            id: Id::null(),
        }.await
    }

    pub async fn timeout<??: TryIntoDelta, F, R> (&self, delta: ??, f: F) -> Result<R, Error>
    where
        F: Future<Output=R>
    {
        let delta = delta.try_into_delta(self.clock.frequency()).ok()
            .expect("equeue: delta overflow");
        AsyncTimeout {
            q: self,
            f: f,
            timeout: self.now() + delta,
            id: Id::null(),
        }.await
    }
}

impl<C: Clock+AsyncSema> Equeue<C> {
    async fn dispatch_async_(&self, delta: Option<Delta>) -> Dispatch {
        // note that some of this is ungracefull copy-pasted from non-async dispatch

        // get the current time
        let mut now = self.now();
        let timeout = delta.map(|delta| now + delta);

        loop {
            // dispatch events
            self.dispatch_ready();

            // was break requested?
            if self.update_break_(|break_| break_.checked_sub(1)) > 0 {
                return Dispatch::Break;
            }

            // should we stop dispatching?
            //
            // note that time could have changed _significantly_
            now = self.now();
            let timeout_left = timeout.map(|timeout| timeout - now);
            if let Some(timeout_left) = timeout_left {
                if timeout_left <= Delta::zero() {
                    return Dispatch::Timeout;
                }
            }

            // ok how long should we sleep for
            //
            // Note that we always try to sleep between slices, this is
            // just to behave nicely in case the system's semaphore implementation
            // does something "clever". Note we also never enter here if
            // ticks is 0 for similar reasons.
            let delta = self.delta_(now)
                .into_iter().chain(timeout_left)
                .min();

            match delta {
                Some(delta) => self.clock.wait_timeout_async(delta).await,
                None => self.clock.wait_async().await,
            }
        }
    }

    #[inline]
    pub async fn dispatch_for_async<??: TryIntoDelta>(&self, delta: ??) -> Dispatch {
        self.dispatch_async_(Some(
            delta.try_into_delta(self.clock.frequency()).ok()
                .expect("equeue: delta overflow")
        )).await
    }

    #[inline]
    pub async fn dispatch_async(&self) -> Dispatch {
        self.dispatch_async_(None).await
    }
}


#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct Usage {
    pub posted: usize,
    pub posted_bytes: usize,
    pub alloced: usize,
    pub alloced_bytes: usize,
    pub free: usize,
    pub free_bytes: usize,
    pub slices: usize,

    pub slab_total: usize,
    pub slab_fragmented: usize,
    pub slab_unused: usize,
    pub buckets: usize,
}

impl<C> Equeue<C> {
    pub fn usage(&self) -> Usage {
        // find slab usage
        let slab_total = self.slab.len();
        let slab_front = self.slab_front.load() as usize * size_of::<udeptr>();
        let slab_back = self.slab_back.load() as usize * size_of::<udeptr>();
        let slab_unused = slab_back - slab_front;

        let mut total = 0usize;
        let mut i = slab_back;
        while let Some(e) = self.slab.get(i) {
            let e = unsafe { &*(e as *const _ as *const EBuf) };

            total += 1;
            i = i.saturating_add(size_of::<EBuf>() + e.size());
        }

        // find pending usage
        let mut posted = 0;
        let mut posted_bytes = 0;
        let mut slices = 0;
        'slices: for head in iter::successors(
            self.queue.load().as_ref(self),
            |head| head.next.load().as_ref(self)
        ) {
            slices += 1;
            for sibling in iter::successors(
                Some(head),
                |sibling| sibling.sibling.load().as_ref(self)
                    .filter(|&sibling| sibling as *const _ != head as *const _)
            ) {
                posted += 1;
                posted_bytes += size_of::<EBuf>() + sibling.size();

                // this is all completely unsynchronized, so we have to set some
                // hard limits to prevent getting stuck in an infinite loop, 
                if posted > total {
                    posted = 1;
                    break;
                }

                if slices > total {
                    slices = 1;
                    break 'slices;
                }
            }
        }

        // find bucket usage
        let buckets = self.buckets();
        let mut free = 0;
        let mut free_bytes = 0;
        for (npw2, head) in buckets.iter().enumerate() {
            for _ in iter::successors(
                head.load().as_ref(self),
                |head| head.sibling.load().as_ref(self)
            ) {
                free += 1;
                free_bytes += size_of::<EBuf>() + (EBuf::ALIGN << npw2);

                // this is all completely unsynchronized, so we have to set some
                // hard limits to prevent getting stuck in an infinite loop, 
                if free > total {
                    free = 1;
                    break;
                }
            }
        }

        // these are best-effort numbers, it's very easy for in-flight events
        // to create numbers that are just flat-out wrong
        //
        // we can at least clamp some of the numbers to reasonable limits
        // to avoid breaking user's code as much as possible
        let posted = min(posted, total);
        let posted_bytes = min(posted_bytes, slab_total.saturating_sub(slab_unused+slab_front));
        let free = min(free, total-posted);
        let free_bytes = min(free_bytes, slab_total.saturating_sub(slab_unused+slab_front)-posted_bytes);

        Usage {
            posted: posted,
            posted_bytes: posted_bytes,
            alloced: total.saturating_sub(posted+free),
            alloced_bytes: slab_total.saturating_sub(posted_bytes+free_bytes+slab_unused+slab_front),
            free: free,
            free_bytes: free_bytes+slab_unused,
            slices: slices,

            slab_total: slab_total,
            slab_fragmented: slab_total.saturating_sub(slab_unused),
            slab_unused: slab_unused,
            buckets: buckets.len(),
        }
    }

    pub fn bucket_usage(&self, buckets: &mut [usize]) {
        // this is just a guess
        let total = self.slab.len() / size_of::<EBuf>();

        for (bucket, head) in buckets.iter_mut()
            .zip(self.buckets())
        {
            let mut count = 0;
            for _ in iter::successors(
                head.load().as_ref(self),
                |head| head.sibling.load().as_ref(self)
            ) {
                count += 1;

                // this is all completely unsynchronized, so we have to set some
                // hard limits to prevent getting stuck in an infinite loop, 
                if count > total {
                    count = 1;
                    break;
                }
            }
            *bucket = count;
        }
    }

    pub fn slice_usage(&self, slices: &mut [usize]) {
        // this is just a guess
        let total = self.slab.len() / size_of::<EBuf>();

        for (slice, head) in slices.iter_mut()
            .zip(
                iter::successors(
                    self.queue.load().as_ref(self),
                    |head| head.next.load().as_ref(self)
                )
            )
        {
            let mut count = 0;
            for sibling in iter::successors(
                Some(head),
                |sibling| sibling.sibling.load().as_ref(self)
                    .filter(|&sibling| sibling as *const _ != head as *const _)
            ) {
                count += 1;

                // this is all completely unsynchronized, so we have to set some
                // hard limits to prevent getting stuck in an infinite loop, 
                if count > total {
                    count = 1;
                    break;
                }
            }
            *slice = count;
        }
    }
}


/// LocalEqueue wrapper, for non-send + non-sync types
///
/// Note we use *const () to force !Send and !Sync, since that's the only
/// way to do this in stable Rust
#[derive(Debug)]
#[repr(transparent)]
pub struct LocalEqueue<
    #[cfg(feature="std")] C=SysClock,
    #[cfg(not(feature="std"))] C,
>(Equeue<C>, PhantomData<*const ()>);

impl<C> LocalEqueue<C> {
    pub fn with_config(config: Config<C>) -> LocalEqueue<C> {
        Self(Equeue::with_config(config), PhantomData)
    }
}

#[cfg(feature="std")]
impl LocalEqueue {
    #[cfg(feature="alloc")]
    pub fn with_size(size: usize) -> LocalEqueue {
        Self(Equeue::with_size(size), PhantomData)
    }

    pub fn with_buffer(buffer: &'static mut [u8]) -> LocalEqueue {
        Self(Equeue::with_buffer(buffer), PhantomData)
    }
}

impl<C: Clock> LocalEqueue<C> {
    pub fn delta_id<??: TryFromDelta>(&self, id: Id) -> Option<??> {
        self.0.delta_id(id)
    }
}

impl<C> LocalEqueue<C> {
    pub fn cancel(&self, id: Id) -> bool {
        self.0.cancel(id)
    }
}

impl<C: Clock+Signal> LocalEqueue<C> {
    pub fn post_id(&self, id: Id) -> bool {
        self.0.post_id(id)
    }
}

impl<C: Clock> LocalEqueue<C> {
    pub fn delta<??: TryFromDelta>(&self) -> Option<??> {
        self.0.delta()
    }

    pub fn dispatch_ready(&self) {
        self.0.dispatch_ready()
    }
}

impl<C: Clock+Sema> LocalEqueue<C> {
    #[inline]
    pub fn dispatch_for<??: TryIntoDelta>(&self, delta: ??) -> Dispatch {
        self.0.dispatch_for(delta)
    }

    #[inline]
    pub fn dispatch(&self) -> Dispatch {
        self.0.dispatch()
    }
}

impl<C: Signal> LocalEqueue<C> {
    pub fn break_(&self) {
        self.0.break_()
    }
}

// Raw allocations
impl<C> LocalEqueue<C> { 
    pub unsafe fn alloc_raw(
        &self,
        layout: Layout,
        cb: fn(*mut u8),
        drop: fn(*mut u8)
    ) -> *mut u8 {
        self.0.alloc_raw(layout, cb, drop)
    }

    pub unsafe fn dealloc_raw(&self, e: *mut u8) {
        self.0.dealloc_raw(e)
    }
}


// A wrapper to fake Send for reusing the Equeue implementation
#[derive(Debug)]
#[repr(transparent)]
struct IgnoreSend<T: ?Sized>(T);

unsafe impl<T: ?Sized> Send for IgnoreSend<T> {}

impl<T: Post> Post for IgnoreSend<T> {
    fn post(&mut self) {
        self.0.post();
    }
}

impl<T: PostOnce> PostOnce for IgnoreSend<T> {
    fn post_once(self) {
        self.0.post_once();
    }
}

impl<'a, T: PostStatic<LocalEvent<'a, T, C>>, C> PostStatic<Event<'a, IgnoreSend<T>, C>> for IgnoreSend<T> {
    fn post_static(self_: Event<'a, IgnoreSend<T>, C>) {
        T::post_static(unsafe { transmute(self_) })
    }
}

impl<T: Future<Output=()>> Future for IgnoreSend<T> {
    type Output = ();
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<()> {
        unsafe { self.map_unchecked_mut(|self_| &mut self_.0) }.poll(cx)
    }
}


/// Non-Send/Sync Event Handle
#[derive(Debug)]
#[repr(transparent)]
pub struct LocalEvent<
    'a,
    T: ?Sized,
    #[cfg(feature="std")] C=SysClock,
    #[cfg(not(feature="std"))] C,
>(Event<'a, T, C>, PhantomData<*const ()>);

impl<'a, T: ?Sized, C> LocalEvent<'a, T, C> {
    pub fn into_raw(self) -> (&'a LocalEqueue<C>, *mut T) {
        let (q, t) = self.0.into_raw();
        (unsafe { &mut *(q as *const _ as *mut LocalEqueue<C>) }, t)
    }

    pub unsafe fn from_raw(q: &'a LocalEqueue<C>, t: *mut T) -> LocalEvent<'a, T, C> {
        LocalEvent(Event::from_raw(&q.0, t), PhantomData)
    }
}

impl<C> LocalEqueue<C> {
    pub fn alloc<'a, T: Post>(&'a self, t: T) -> Result<LocalEvent<'a, T, C>, Error> {
        let (q, e) = self.0.alloc(IgnoreSend(t))?.into_raw();
        Ok(LocalEvent(unsafe { Event::from_raw(q, e as *mut T) }, PhantomData))
    }

    pub fn alloc_once<'a, T: PostOnce>(&'a self, t: T) -> Result<LocalEvent<'a, T, C>, Error> {
        let (q, e) = self.0.alloc_once(IgnoreSend(t))?.into_raw();
        Ok(LocalEvent(unsafe { Event::from_raw(q, e as *mut T) }, PhantomData))
    }

    pub fn alloc_static<'a, T: PostStatic<LocalEvent<'a, T, C>>>(&'a self, t: T) -> Result<LocalEvent<'a, T, C>, Error> {
        let (q, e) = self.0.alloc_static(IgnoreSend(t))?.into_raw();
        Ok(LocalEvent(unsafe { Event::from_raw(q, e as *mut T) }, PhantomData))
    }
}

impl<C: Clock+Signal> LocalEqueue<C> {
    pub fn alloc_future<'a, T: Future<Output=()>>(&'a self, t: T) -> Result<LocalEvent<'a, T, C>, Error> {
        let (q, e) = self.0.alloc_future(IgnoreSend(t))?.into_raw();
        Ok(LocalEvent(unsafe { Event::from_raw(q, e as *mut T) }, PhantomData))
    }
}

impl<'a, T: ?Sized, C: Clock+Signal> LocalEvent<'a, T, C> {
    pub fn delay<??: TryIntoDelta>(self, delay: ??) -> Self {
        LocalEvent(self.0.delay(delay), PhantomData)
    }

    pub fn period<??: TryIntoDelta>(self, period: Option<??>) -> Self {
        LocalEvent(self.0.period(period), PhantomData)
    }

    pub fn static_(self, static_: bool) -> Self {
        LocalEvent(self.0.static_(static_), PhantomData)
    }

    pub fn into_id(self) -> Id {
        self.0.into_id()
    }

    pub fn into_handle(self) -> Handle<'a, C> {
        self.0.into_handle()
    }

    pub fn post(self) -> Id {
        self.0.post()
    }

    pub fn post_handle(self) -> Handle<'a, C> {
        self.0.post_handle()
    }
}

impl<T: ?Sized, C> Deref for LocalEvent<'_, T, C> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.deref()
    }
}

impl<T: ?Sized, C> DerefMut for LocalEvent<'_, T, C> {
    fn deref_mut(&mut self) -> &mut T {
        self.0.deref_mut()
    }
}

impl<T: ?Sized, C> AsRef<T> for LocalEvent<'_, T, C> {
    fn as_ref(&self) -> &T {
        self.0.as_ref()
    }
}

impl<T: ?Sized, C> AsMut<T> for LocalEvent<'_, T, C> {
    fn as_mut(&mut self) -> &mut T {
        self.0.as_mut()
    }
}

impl<C: Clock+Signal> LocalEqueue<C> {
    pub fn call<F: PostOnce>(
        &self,
        cb: F
    ) -> Result<Id, Error> {
        Ok(
            self.alloc_once(cb)?
                .post()
        )
    }

    pub fn call_in<??: TryIntoDelta, F: PostOnce>(
        &self,
        delay: ??,
        cb: F
    ) -> Result<Id, Error> {
        Ok(
            self.alloc_once(cb)?
                .delay(delay)
                .post()
        )
    }

    pub fn call_every<??: TryIntoDelta, F: Post>(
        &self,
        period: ??,
        cb: F
    ) -> Result<Id, Error> {
        let period = period.try_into_delta(self.0.clock.frequency()).ok()
            .expect("equeue: delta overflow");
        Ok(
            self.alloc(cb)?
                .delay(period)
                .period(Some(period))
                .post()
        )
    }

    pub fn run<F: Future<Output=()>>(
        &self,
        cb: F
    ) -> Result<Id, Error> {
        Ok(
            self.alloc_future(cb)?
                .post()
        )
    }

    pub fn call_handle<'a, F: PostOnce>(
        &'a self,
        cb: F
    ) -> Result<Handle<'a, C>, Error> {
        self.call(cb).map(|id| Handle::new(&self.0, id))
    }

    pub fn call_in_handle<'a, ??: TryIntoDelta, F: PostOnce>(
        &'a self,
        delay: ??,
        cb: F
    ) -> Result<Handle<'a, C>, Error> {
        self.call_in(delay, cb).map(|id| Handle::new(&self.0, id))
    }

    pub fn call_every_handle<'a, ??: TryIntoDelta, F: Post>(
        &'a self,
        period: ??,
        cb: F
    ) -> Result<Handle<'a, C>, Error> {
        self.call_every(period, cb).map(|id| Handle::new(&self.0, id))
    }

    pub fn run_handle<'a, F: Future<Output=()>>(
        &'a self,
        cb: F
    ) -> Result<Handle<'a, C>, Error> {
        self.run(cb).map(|id| Handle::new(&self.0, id))
    }
}

impl<C: Clock+Signal> LocalEqueue<C> {
    pub async fn yield_(&self) {
        self.0.yield_().await
    }

    pub async fn sleep<??: TryIntoDelta>(&self, delta: ??) -> Result<(), Error> {
        self.0.sleep(delta).await
    }

    pub async fn timeout<??: TryIntoDelta, F, R>(&self, delta: ??, f: F) -> Result<R, Error>
    where
        F: Future<Output=R>
    {
        self.0.timeout(delta, f).await
    }
}

impl<C: Clock+AsyncSema> LocalEqueue<C> {
    #[inline]
    pub async fn dispatch_for_async<??: TryIntoDelta>(&self, delta: ??) -> Dispatch {
        self.0.dispatch_for_async(delta).await
    }

    #[inline]
    pub async fn dispatch_async(&self) -> Dispatch {
        self.0.dispatch_async().await
    }
}

impl<C> LocalEqueue<C> {
    pub fn usage(&self) -> Usage {
        self.0.usage()
    }

    pub fn bucket_usage(&self, buckets: &mut [usize]) {
        self.0.bucket_usage(buckets);
    }

    pub fn slice_usage(&self, slices: &mut [usize]) {
        self.0.slice_usage(slices);
    }
}
