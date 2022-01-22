
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

use either::Either;
use either::Left;
use either::Right;

mod util;
pub mod sys;
use util::*;
use sys::*;

mod traits;
pub use traits::*;

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
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(transparent)]
struct Tick(utick);

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
        f.debug_tuple("Delta")
            .field(&self.uticks())
            .finish()
    }
}

impl Delta {
    pub const fn zero() -> Delta {
        Self(unsafe { NonZeroItick::new_unchecked(!0) })
    }

    pub const fn new(t: itick) -> Option<Delta> {
        if t >= 0 {
            Some(unsafe { Self::new_unchecked(t) })
        } else {
            None
        }
    }

    pub const unsafe fn new_unchecked(t: itick) -> Delta {
        Self(NonZeroItick::new_unchecked(!t))
    }

    pub const fn iticks(self) -> itick {
        !self.0.get()
    }

    pub const fn uticks(self) -> utick {
        // completely safe since we know we have no negative values
        self.iticks() as utick
    }

    // we store some deltas as ticks to reuse memory, so we need this,
    // it's not worth the noise to create a union
    const fn as_tick(self) -> Tick {
        Tick::new(self.uticks())
    }
}

impl PartialOrd for Delta {
    fn partial_cmp(&self, other: &Delta) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Delta {
    fn cmp(&self, other: &Delta) -> cmp::Ordering {
        self.uticks().cmp(&other.uticks())
    }
}

impl TryIntoDelta for Delta {
    type Error = Infallible;
    fn try_into_delta(self, _: utick) -> Result<Delta, Self::Error> {
        Ok(self)
    }
}

impl TryFromDelta for Delta {
    type Error = Infallible;
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

impl<T, S: AtomicStorage> Atomic<T, S> {
    fn new(t: T) -> Self {
        debug_assert!(size_of::<T>() <= size_of::<S::U>());
        Self(
            S::new(unsafe { *(&t as *const _ as *const S::U) }),
            PhantomData
        )
    }

    fn load(&self) -> T where T: Copy {
        unsafe { *(&self.0.load() as *const _ as *const T) }
    }

    fn store(&self, v: T) {
        self.0.store(unsafe { *(&v as *const _ as *const S::U) })
    }
}


/// Slab-internal pointer, with internalized generation count
#[derive(Copy, Clone, Eq, PartialEq)]
#[repr(transparent)]
struct Eptr(ueptr);

impl Debug for Eptr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // these really need to be in hex to be readable
        f.debug_tuple("Eptr")
            .field(&format_args!("{:#x}", self.0))
            .finish()
    }
}

impl Eptr {
    const fn null() -> Eptr {
        Eptr(0)
    }

    fn from_ebuf<C>(q: &Equeue<C>, e: &Ebuf) -> Eptr {
        Eptr(unsafe {
            ((e as *const Ebuf as *const u8)
                .offset_from(q.slab.as_ptr())
                as usize
                / align_of::<Ebuf>())
                as ueptr
        })
    }

    fn as_ptr<C>(&self, q: &Equeue<C>) -> *const Ebuf {
        match self.0 {
            0 => ptr::null(),
            _ => (
                &q.slab[self.0 as usize * align_of::<Ebuf>()]
                    as *const _ as *const Ebuf
            )
        }
    }

    fn as_ref<'a, C>(&self, q: &'a Equeue<C>) -> Option<&'a Ebuf> {
        unsafe { self.as_ptr(q).as_ref() }
    }
}

/// A marked eptr, used to double-check non-locking parts
/// of the data structures
#[derive(Copy, Clone, Eq, PartialEq)]
struct MarkedEptr {
    gen: ugen,
    eptr: Eptr,
}

impl Debug for MarkedEptr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // these really need to be in hex to be readable
        f.debug_tuple("MarkedEptr")
            .field(&self.gen)
            .field(&format_args!("{:#x}", self.eptr.0))
            .finish()
    }
}

impl MarkedEptr {
    const fn null() -> MarkedEptr {
        MarkedEptr {
            gen: 0,
            eptr: Eptr::null(),
        }
    }

    fn from_ebuf<C>(q: &Equeue<C>, e: &mut Ebuf) -> MarkedEptr {
        MarkedEptr {
            gen: 0,
            eptr: Eptr::from_ebuf(q, e),
        }
    }

    fn as_ptr<C>(&self, q: &Equeue<C>) -> *const Ebuf {
        self.eptr.as_ptr(q)
    }

    fn as_ref<'a, C>(&self, q: &'a Equeue<C>) -> Option<&'a Ebuf> {
        self.eptr.as_ref(q)
    }

    // test if mark matches expected gen from src MarkedEptr
    fn cp_mark(self, src: MarkedEptr) -> MarkedEptr {
        MarkedEptr {
            gen: src.gen,
            eptr: self.eptr,
        }
    }

    fn inc_mark(self) -> MarkedEptr {
        MarkedEptr {
            gen: self.gen.wrapping_add(1),
            eptr: self.eptr,
        }
    }
}

// interactions with atomics
impl<S: AtomicStorage> Atomic<MarkedEptr, S> {
    fn store_marked(&self, src: MarkedEptr, new: MarkedEptr) -> MarkedEptr {
        let eptr = new.cp_mark(src);
        self.store(eptr);
        eptr
    }

    fn store_inc_marked(&self, src: MarkedEptr, new: MarkedEptr) -> MarkedEptr {
        let eptr = new.cp_mark(src).inc_mark();
        self.store(eptr);
        eptr
    }
}


/// Several event fields are crammed in here to avoid wasting space
#[derive(Copy, Clone, Eq, PartialEq)]
struct Einfo(ueptr);

impl Debug for Einfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // these really need to be in hex to be readable
        f.debug_tuple("Einfo")
            .field(&format_args!("{:#x}", self.0))
            .finish()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Pending {
    Alloced  = 0,
    Pending  = 2,
    InFlight = 4,
    Nested   = 6,
    Canceled = 7,
}

impl Pending {
    fn from_ueptr(pending: ueptr) -> Pending {
        match pending {
            0 | 1 => Pending::Alloced,
            2 | 3 => Pending::Pending,
            4 | 5 => Pending::InFlight,
            6     => Pending::Nested,
            7     => Pending::Canceled,
            _ => unreachable!(),
        }
    }

    fn as_ueptr(&self) -> ueptr {
        match self {
            Pending::Alloced  => 0,
            Pending::Pending  => 2,
            Pending::InFlight => 4,
            Pending::Nested   => 6,
            Pending::Canceled => 7,
        }
    }
}

impl Einfo {
    fn new(id: ugen, pending: Pending, static_: bool, npw2: u8) -> Einfo {
        Einfo(
            ((id as ueptr) << 8*size_of::<ugen>())
            | (pending.as_ueptr() << (8*size_of::<ugen>()-3))
            | ((!static_ as ueptr) << (8*size_of::<ugen>()-3))
            | (npw2 as ueptr)
        )
    }

    fn id(&self) -> ugen {
        (self.0 >> 8*size_of::<ugen>()) as ugen
    }

    fn pending(&self) -> Pending {
        Pending::from_ueptr(0x7 & (self.0 >> (8*size_of::<ugen>()-3)))
    }

    fn static_(&self) -> bool {
        // static is inverted so we can mask it with canceled statuses
        !(self.0 & (1 << (8*size_of::<ugen>()-3)) != 0)
    }

    fn npw2(&self) -> u8 {
        (self.0 & ((1 << (8*size_of::<ugen>()-3)) - 1)) as u8
    }

    fn inc_id(self) -> Einfo {
        Einfo(self.0.wrapping_add(1 << 8*size_of::<ugen>()))
    }

    fn set_pending(self, pending: Pending) -> Einfo {
        // there is one incompatible state here
        debug_assert!(pending != Pending::Nested || self.static_());
        Einfo(
            (self.0 & !(0x6 << (8*size_of::<ugen>()-3)))
            | (pending.as_ueptr() << (8*size_of::<ugen>()-3))
        )
    }

    fn set_static(self, static_: bool) -> Einfo {
        // static is inverted so we can mask it with canceled statuses
        if !static_ {
            Einfo(self.0 | (1 << (8*size_of::<ugen>()-3)))
        } else {
            Einfo(self.0 & !(1 << (8*size_of::<ugen>()-3)))
        }
    }
}


/// Internal event header
#[derive(Debug)]
struct Ebuf {
    next: Atomic<MarkedEptr, AtomicUdeptr>,
    next_back: Atomic<Eptr, AtomicUeptr>,
    sibling: Atomic<Eptr, AtomicUeptr>,
    sibling_back: Atomic<Eptr, AtomicUeptr>,
    info: Atomic<Einfo, AtomicUeptr>,

    cb: Option<fn(*mut u8)>,
    drop: Option<fn(*mut u8)>,
    target: Tick,
    period: Option<Delta>,
    // TODO can we store this somewhere else?
    q: *const Equeue,
}

impl Ebuf {
    fn as_eptr<C>(&self, q: &Equeue<C>) -> Eptr {
        Eptr::from_ebuf(q, self)
    }

    fn as_marked_eptr<C>(&mut self, q: &Equeue<C>) -> MarkedEptr {
        MarkedEptr::from_ebuf(q, self)
    }

    // info access
    fn npw2(&self) -> u8 {
        self.info.load().npw2()
    }

    fn size(&self) -> usize {
        align_of::<Ebuf>() << self.npw2()
    }

    // access to the trailing buffer
    fn data_ptr<T>(&self) -> *const T {
        unsafe { (self as *const Ebuf).add(1) as *const T }
    }

    fn data_mut_ptr<T>(&mut self) -> *mut T {
        unsafe { (self as *mut Ebuf).add(1) as *mut T }
    }

    unsafe fn data_ref<'a, T>(&'a self) -> &'a T {
        &*self.data_ptr()
    }

    unsafe fn data_mut<'a, T>(&'a mut self) -> &'a mut T {
        &mut *self.data_mut_ptr()
    }

    unsafe fn from_data_mut_ptr<'a, T>(ptr: *mut T) -> Option<&'a mut Ebuf> {
        if !ptr.is_null() {
            Some(&mut *(ptr as *mut Ebuf).sub(1))
        } else {
            None
        }
    }

    // we can "claim" an Ebuf once we remove it from shared structures,
    // the claim is unsafe, but afterwards we can leverage Rust's type
    // system to know whether or not we have exclusive access to the Ebuf
    unsafe fn claim<'a>(&'a self) -> &'a mut Ebuf {
        &mut *(self as *const Ebuf as *mut Ebuf)
    }
}

// some convenience extensions to Option<&Ebuf>
trait OptionEbuf<'a> {
    fn as_eptr<C>(&self, q: &Equeue<C>) -> Eptr;
}

impl<'a> OptionEbuf<'a> for Option<&'a Ebuf> {
    fn as_eptr<C>(&self, q: &Equeue<C>) -> Eptr {
        match self {
            Some(e) => e.as_eptr(q),
            None => Eptr::null(),
        }
    }
}


/// Event queue struct
#[derive(Debug)]
pub struct Equeue<C=SysClock> {
    // memory management
    slab: &'static [u8],
    slab_front: Atomic<ueptr, AtomicUeptr>,
    slab_back: Atomic<ueptr, AtomicUeptr>,
    #[cfg(feature="alloc")] alloced: bool,

    // queue management
    queue: Atomic<MarkedEptr, AtomicUdeptr>,
    dequeue: Atomic<MarkedEptr, AtomicUdeptr>,
    break_: Atomic<bool, AtomicUeptr>,
    precision: u8,

    // other things
    clock: C,
    lock: SysLock,
}

/// Event queue configuration
#[derive(Debug)]
pub struct Config<C=SysClock> {
    pub clock: C,
    pub precision: Option<u8>,

    pub buffer: Buffer,
}

#[derive(Debug)]
pub enum Buffer {
    #[cfg(feature="alloc")] Alloc(usize),
    Static(&'static mut [u8]),
}

// assert that we implement Send + Sync
#[allow(unconditional_recursion)] fn assert_send<T: Send>() -> ! { assert_send::<Equeue>() }
#[allow(unconditional_recursion)] fn assert_sync<T: Sync>() -> ! { assert_sync::<Equeue>() }

impl<C> Equeue<C> {
    pub fn with_config(config: Config<C>) -> Equeue<C> {
        // some sanity checks
        debug_assert!(align_of::<Ebuf>() >= align_of::<usize>());
        debug_assert_eq!(size_of::<Option<Delta>>(), size_of::<itick>());

        let (buffer, alloced) = match config.buffer {
            #[cfg(feature="alloc")]
            Buffer::Alloc(size) => {
                let size = aligndown(size, align_of::<Ebuf>());
                let layout = Layout::from_size_align(size, align_of::<Ebuf>()).unwrap();
                let buffer = unsafe { alloc(layout) };
                assert!(!buffer.is_null());

                let buffer = unsafe { slice::from_raw_parts_mut(buffer, size) };
                (buffer, true)
            }
            Buffer::Static(buffer) => {
                (buffer, false)
            }
        };

        // align buffer
        let range = buffer.as_ptr_range();
        let start = alignup(range.start as usize, align_of::<Ebuf>()) - range.start as usize;
        let end = aligndown(range.end as usize, align_of::<Ebuf>()) - range.start as usize;
        let buffer = buffer.get_mut(start..end).unwrap();

        // go ahead and zero our buffer, this makes it easier to manage bucket
        // allocation, which needs to be null the moment a bucket is allocated
        buffer.fill(0);

        Equeue {
            slab: buffer,
            #[cfg(feature="alloc")] alloced: alloced,
            // there's already a bit of finagling here to fit sizes in ueptrs,
            // slab_front is used for bytes, but should never exceed ~log2(width)
            // while slab_back is used for ebufs, which have a larger alignment
            slab_front: Atomic::new(0),
            slab_back: Atomic::new(
                ueptr::try_from(buffer.len() / align_of::<Ebuf>()).unwrap()
            ),

            queue: Atomic::new(MarkedEptr::null()),
            dequeue: Atomic::new(MarkedEptr::null()),
            break_: Atomic::new(false),
            precision: config.precision.unwrap_or(PRECISION),

            clock: config.clock,
            lock: SysLock::new(),
        }
    }
}

#[cfg(feature="std")]
impl Equeue {
    pub fn with_size(size: usize) -> Equeue {
        Equeue::with_config(Config {
            clock: SysClock::new(),
            precision: None,
            buffer: Buffer::Alloc(size),
        })
    }

    pub fn with_buffer(buffer: &'static mut [u8]) -> Equeue {
        Equeue::with_config(Config {
            clock: SysClock::new(),
            precision: None,
            buffer: Buffer::Static(buffer),
        })
    }
}

impl<C> Drop for Equeue<C> {
    fn drop(&mut self) {
        // make sure we call drop on any pending events
        //
        // we can't just traverse the queue here, because alloced but unpended
        // events aren't there until pend is called, this includes
        // intermediary states for static events/futures.
        //
        // it's up to dealloc_ to make sure drop is cleared after called
        let mut i = self.slab_back.load() as usize * align_of::<Ebuf>();
        while let Some(e) = self.slab.get(i) {
            let e = unsafe { &*(e as *const _ as *const Ebuf) };
            let e = unsafe { e.claim() };

            if let Some(drop) = e.drop {
                drop(e.data_mut_ptr());
            }
            e.drop = None;

            // just some extra precautions against poorly written wakers
            e.info.store(e.info.load().inc_id());

            i = i.saturating_add(size_of::<Ebuf>() + e.size());
        }

        // free allocated buffer?
        #[cfg(feature="alloc")]
        if self.alloced {
            let layout = Layout::from_size_align(self.slab.len(), align_of::<Ebuf>()).unwrap();
            unsafe { dealloc(self.slab.as_ptr() as *mut u8, layout) };
        }
    }
}

impl<C> Equeue<C> {
    fn contains(&self, e: &Ebuf) -> bool {
        self.slab.as_ptr_range()
            .contains(&(e.deref() as *const _ as *const u8))
    }

    fn buckets<'a>(&'a self) -> &'a [Atomic<Eptr, AtomicUeptr>] {
        let slab_front = self.slab_front.load() as usize;
        unsafe {
            slice::from_raw_parts(
                self.slab.as_ptr() as *const Atomic<Eptr, AtomicUeptr>,
                slab_front / size_of::<Atomic<Eptr, AtomicUeptr>>()
            )
        }
    }

    // Memory management
    fn alloc_<'a>(&'a self, layout: Layout) -> Result<&'a mut Ebuf, Error> {
        // this looks arbitrary, but Ebuf should have a pretty reasonable
        // alignment since it contains both function pointers and AtomicUdeptrs
        assert!(layout.align() <= align_of::<Ebuf>(),
            "unable to alloc alignment {} > {}",
            layout.align(), align_of::<Ebuf>()
        );

        // find best bucket
        let npw2 = npw2(
            alignup(layout.size(), align_of::<Ebuf>())
                / align_of::<Ebuf>()
        );

        // do we have an allocation in our buckets? we don't look
        // at larger buckets because those are likely to be reused, we don't
        // want to starve larger events with smaller events
        if let Some(bucket) = self.buckets().get(npw2 as usize) {
            // try to take an event from a bucket
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

        let new_slab_back = {
            let guard = self.lock.lock();

            // check if we even have enough memory available, we allocate both
            // an event and maybe some buckets if we don't have enough
            let slab_front = self.slab_front.load() as usize;
            let slab_back = self.slab_back.load() as usize * align_of::<Ebuf>();
            let new_slab_front = max(
                (npw2 as usize + 1)*size_of::<Eptr>(),
                slab_front
            );
            let new_slab_back = slab_back.saturating_sub(
                size_of::<Ebuf>() + (align_of::<Ebuf>() << npw2)
            );

            if new_slab_front > new_slab_back {
                return Err(Error::NoMem);
            }

            // actually commit our allocation
            if new_slab_front > slab_front {
                self.slab_front.store(
                    ueptr::try_from(new_slab_front).unwrap()
                );
            }

            debug_assert!(new_slab_back < slab_back);
            self.slab_back.store(
                ueptr::try_from(new_slab_back / align_of::<Ebuf>()).unwrap()
            );
            new_slab_back
        };

        unsafe {
            let e = &self.slab[new_slab_back]
                as *const u8 as *const Ebuf as *mut Ebuf;
            e.write(Ebuf {
                next: Atomic::new(MarkedEptr::null()),
                next_back: Atomic::new(Eptr::null()),
                sibling: Atomic::new(Eptr::null()),
                sibling_back: Atomic::new(Eptr::null()),
                info: Atomic::new(Einfo::new(0, Pending::InFlight, false, npw2)),

                cb: None,
                drop: None,
                target: Tick::new(0),
                period: None,
                q: ptr::null(),
            });

            Ok(&mut *e)
        }
    }

    fn dealloc_(&self, e: &mut Ebuf) {
        debug_assert!(self.contains(e));

        // make sure to run destructors if assigned, and clear destructors
        // so we don't double-drop if the equeue is itself dropped
        if let Some(drop) = e.drop {
            drop(e.data_mut_ptr());
        }
        e.drop = None;

        // we can load buckets here because it can never shrink
        let bucket = &self.buckets()[e.npw2() as usize];

        // add our event to a bucket, while also incrementing our
        // generation count
        {   let guard = self.lock.lock();

            // give our event a new id
            let info = e.info.load();
            debug_assert_ne!(info.pending(), Pending::Pending);
            e.info.store(
                info.inc_id()
                    .set_pending(Pending::InFlight)
                    .set_static(false)
            );

            // push onto bucket
            let siblingptr = bucket.load();
            debug_assert_ne!(e as *const _, siblingptr.as_ptr(self));
            e.sibling.store(siblingptr);
            bucket.store(e.as_eptr(self));
        }
    }
}

// Queue management
impl<C> Equeue<C> {
    #[must_use]
    fn enqueue_<'a>(
        &self,
        e: &'a mut Ebuf,
        now: Tick,
        target: Tick
    ) -> Result<bool, &'a mut Ebuf> {
        let mut e = e;
        debug_assert!(e.cb.is_some());
        e.target = target;

        'retry: loop {
            let dequeue_mark = self.dequeue.load().gen;

            // find insertion point
            let mut back = None;
            let mut sibling = None;

            let mut tailptr = self.queue.load();
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

            // prepare event before locking
            match sibling {
                None => {
                    // insert a new slice
                    e.next.store_marked(e.next.load(), tailptr);
                    e.next_back.store(back.as_eptr(self));
                    e.sibling.store(e.as_eptr(self));
                    e.sibling_back.store(e.as_eptr(self));
                }
                Some(sibling) => {
                    // push onto existing slice
                    e.next.store_marked(e.next.load(), MarkedEptr::null());
                    e.next_back.store(Eptr::null());
                    e.sibling.store(sibling.as_eptr(self));
                }
            }

            // try to insert
            {   let guard = self.lock.lock();

                // are we trying to enqueue an event that's already been canceled?
                //
                // this may seem seem like a weird place for this check, but
                // it's the only place we lock before re-enqueueing periodic events
                let info = e.info.load();
                match info.pending() {
                    Pending::Alloced => {},
                    Pending::Pending => return Ok(false),
                    Pending::InFlight => {},
                    Pending::Nested => {
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
                    Pending::Canceled => return Err(e),
                }

                // did someone already change our tailsrc? dequeue iteration? restart
                if tailsrc.load() != tailptr
                    || self.dequeue.load().gen != dequeue_mark
                {
                    continue 'retry;
                }

                // found our insertion point, now lets try to insert
                let change = match sibling {
                    None => {
                        // insert a new slice
                        tailsrc.store_marked(tailptr, e.as_marked_eptr(self));
                        if let Some(next) = tailptr.as_ref(self) {
                            next.next_back.store(e.as_eptr(self));
                        }

                        tailsrc as *const _ == &self.queue as *const _
                    }
                    Some(sibling) => {
                        // push onto existing slice

                        // the real need for locking here is to make sure all of
                        // the back-references are correct atomically
                        let sibling_back = sibling.sibling_back.load()
                            .as_ref(self).unwrap();
                        sibling.sibling_back.store(e.as_eptr(self));
                        
                        sibling_back.sibling.store(e.as_eptr(self));
                        e.sibling_back.store(sibling_back.as_eptr(self));

                        false
                    }
                };

                // mark as pending here, enabling removals
                e.info.store(info.set_pending(Pending::Pending));
                return Ok(change)
            }
        }
    }

    fn unqueue_<'a>(
        &'a self,
        e: Either<(&'a Ebuf, ugen), ugen>,
        npending: Pending
    ) -> (bool, Option<&'a mut Ebuf>) {
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
                    if info.id() != id {
                        return (false, None);
                    }

                    (e, info)
                }
                Right(mark) => {
                    // no event specified, pop from dequeue if our mark matches
                    let dequeueptr = self.dequeue.load();
                    let e = match dequeueptr.as_ref(self) {
                        Some(e) if dequeueptr.gen == mark => e,
                        _ => return (false, None),
                    };

                    (e, e.info.load())
                }
            };

            // a bit different logic here, we can cancel periodic events, but
            // we can't reclaim the memory if it's in the middle of executing
            match info.pending() {
                Pending::Pending => {
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
                            if headptr.as_ptr(self) == e as *const _ {
                                self.queue.store_marked(headptr, nextptr);
                            }
                            if deheadptr.as_ptr(self) == e as *const _ {
                                self.dequeue.store_marked(deheadptr, nextptr);
                            }
                            if let Some(next_back) = next_back {
                                next_back.next.store_marked(next_back.next.load(), nextptr);
                            }
                            if let Some(next) = next {
                                next.next_back.store(next_back.as_eptr(self));
                            }
                        } else {
                            // remove from siblings

                            // we need this claim here to notate that it's safe to create
                            // a markedptr from sibling, this loads the generation count from
                            // the sibling's next pointer, which could be a race condition
                            // if we aren't locked
                            let sibling = unsafe { sibling.claim() };
                            sibling.next.store_marked(sibling.next.load(), nextptr);
                            sibling.next_back.store(next_back.as_eptr(self));

                            // update next_back's next/queue head first to avoid invalidating traversals
                            if headptr.as_ptr(self) == e as *const _ {
                                self.queue.store_marked(headptr, sibling.as_marked_eptr(self));
                            }
                            if deheadptr.as_ptr(self) == e as *const _ {
                                self.dequeue.store_marked(deheadptr, sibling.as_marked_eptr(self));
                            }
                            if let Some(next_back) = next_back {
                                next_back.next.store_marked(next_back.next.load(), sibling.as_marked_eptr(self));
                            }
                            if let Some(next) = next {
                                next.next_back.store(sibling.as_eptr(self));
                            }
                        }
                    }

                    sibling_back.sibling.store(sibling.as_eptr(self));
                    sibling.sibling_back.store(sibling_back.as_eptr(self));

                    // mark as removed
                    e.next.store_inc_marked(nextptr, MarkedEptr::null());
                    // mark as not-pending
                    e.info.store(e.info.load().set_pending(npending));
                    
                    // note we are responsible for the memory now
                    (true, Some(unsafe { e.claim() }))
                }
                Pending::Alloced => {
                    // alloced is a weird one, if we end up here, we just need
                    // to claim the event, and since we ensure no ids coexist
                    // mutable references to events at the type-level, we can
                    // be sure we have exclusive access
                    e.info.store(e.info.load().set_pending(npending));
                    (true, Some(unsafe { e.claim() }))
                }
                Pending::InFlight | Pending::Nested => {
                    // if we're periodic/static and currently executing best we
                    // can do is mark the event so it isn't re-enqueued
                    e.info.store(e.info.load().set_pending(npending));
                    (info.static_(), None)
                }
                Pending::Canceled => {
                    (false, None)
                }
            }
        }
    }

    fn dequeue_<'a>(
        &'a self,
        now: Tick
    ) -> impl Iterator<Item=&'a mut Ebuf> + 'a {
        let mark = 'retry: loop {
            // dispatch already in progress? let's try to help out
            let deheadptr = self.dequeue.load();
            if deheadptr.as_ref(self).is_some() {
                break deheadptr.gen;
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
            {   let guard = self.lock.lock();

                // did someone already change our tailsrc? dequeue? queue? restart
                if tailsrc.load() != tailptr
                    || self.dequeue.load() != deheadptr
                    || self.queue.load() != headptr
                {
                    continue 'retry;
                }

                // point dequeue to the head of ready events
                self.dequeue.store_inc_marked(deheadptr, headptr);
                // point queue to the tail of ready events
                self.queue.store_marked(headptr, tailptr);

                // cut our unrolled queue
                back.unwrap().next.store_inc_marked(tailptr, MarkedEptr::null());
                if let Some(tail) = tailptr.as_ref(self) {
                    tail.next_back.store(Eptr::null());
                }

                break deheadptr.inc_mark().gen;
            }
        };

        // unqueue from the dequeue list an event at a time
        Right(iter::from_fn(move || {
            self.unqueue_(Right(mark), Pending::InFlight).1
        }))
    }
}

impl<C: Clock> Equeue<C> {
    fn now(&self) -> Tick {
        Tick::new(self.clock.now())
    }

    // How long until event executes?
    fn delta_(&self, id: Id) -> Option<Delta> {
        let e = match id.as_ref(self) {
            Some(e) => e,
            None => return None,
        };

        // load target/period first so we can be sure these
        // don't change after we read id
        let target = e.target;
        let period = e.period;
        let info = e.info.load();
        if info.id() != id.id {
            return None;
        }

        // note that periodic events are always pending, we load period
        // first since it is not necessarily atomic
        match info.pending() {
            Pending::Alloced => None,
            Pending::Pending => Some(target - self.now()),
            Pending::InFlight => period,
            Pending::Nested => Some(Delta::zero()),
            Pending::Canceled => None,
        }
    }

    pub fn delta<Δ: TryFromDelta>(&self, id: Id) -> Option<Δ> {
        self.delta_(id)
            .map(|delta|
                Δ::try_from_delta(delta, self.clock.frequency()).ok()
                    .expect("delta overflow in equeue")
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
        if info.id() != id.id || info.pending() == Pending::Canceled {
            return false;
        }

        let (canceled, e) = self.unqueue_(Left((e, id.id)), Pending::Canceled);

        if let Some(e) = e {
            // make sure to clean up memory
            self.dealloc_(e);
        }

        canceled
    }
}

impl<C: Clock+Sema> Equeue<C> {
    // Central post function
    fn post_(&self, e: &mut Ebuf, delta: Delta) {
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

    // repost an static event which may already be pending
    pub fn pend(&self, id: Id) -> bool {
        let e = match id.as_ref(self) {
            Some(e) => e,
            None => return false,
        };

        let info = e.info.load();
        if info.id() != id.id {
            return false;
        }

        loop {
            // The main difference between pend and post is that we may
            // be pending an event that's already pending. This means several
            // more corner cases to handle.
            let now = self.now();
            let reenqueue = {
                let guard = self.lock.lock();

                // still the same event?
                let info = e.info.load();
                if info.id() != id.id {
                    return false;
                }

                match info.pending() {
                    Pending::Alloced => {
                        // if we're alloced we can just enqueue, make sure to mark
                        // and claim the event first
                        e.info.store(info.set_pending(Pending::InFlight));
                        Left(unsafe { e.claim() })
                    }
                    Pending::Pending if now < e.target => {
                        // we're pending, but in the future, we want to move this
                        // to execute immediately
                        Right(e)
                    }
                    Pending::InFlight if info.static_() => {
                        // someone else is dispatching, just make sure we mark that
                        // we are interested in the event being repended
                        e.info.store(info.set_pending(Pending::Nested));
                        return true;
                    }
                    Pending::Pending | Pending::Nested => {
                        // do nothing, the event is already pending
                        return true;
                    }
                    Pending::InFlight | Pending::Canceled => {
                        return false;
                    }
                }
            };

            match reenqueue {
                Left(e) => {
                    // reenqueue
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
                Right(e) => {
                    // try to unqueue and continue the loop to reenqueue sooner
                    self.unqueue_(Left((e, id.id)), Pending::InFlight);
                }
            }
        }
    }
}

impl<C: Clock> Equeue<C> {
    // find time until next event without locking
    //
    // note this is clamped to 0 at minimum
    fn next_delta_(&self, now: Tick) -> Option<Delta> {
        'retry: loop {
            // wait, if break is requested we need to process
            // it immediately
            if self.break_.load() {
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

    pub fn next_delta<Δ: TryFromDelta>(&self) -> Option<Δ> {
        self.next_delta_(self.now())
            .map(|delta|
                Δ::try_from_delta(delta, self.clock.frequency()).ok()
                    .expect("delta overflow in equeue")
            )
    }
}

impl<C: Clock+Sema> Equeue<C> {
    // Central dispatch function
    fn dispatch_(&self, delta: Option<Delta>) -> Dispatch {
        // get the current time
        let mut now = self.now();
        let timeout = delta.map(|delta| now + delta);

        loop {
            // get a slice to dispatch
            for e in self.dequeue_(now) {
                // load id here so we can tell if it changes (which happens if the
                // event is reclaimed in dispatch)
                let id = e.info.load().id();

                // dispatch!
                (e.cb.unwrap())(e.data_mut_ptr());

                let info = e.info.load();
                let e = if id != info.id() {
                    // deallocated while dispatching?
                    None
                } else if let Some(period) = e.period {
                    // if periodic, reenqueue, unless we get canceled
                    let now = self.now();
                    self.enqueue_(e, now, now.imprecise_add(
                        period,
                        self.precision
                    )).err()
                } else if info.static_() {
                    // if static, try to mark as no-longer pending, but
                    // note we could be canceled or recursively pended
                    let (reenqueue, e) = {
                        let guard = self.lock.lock();
                        let info = e.info.load();
                        match info.pending() {
                            Pending::InFlight => {
                                e.info.store(info.set_pending(Pending::Alloced));
                                (None, None)
                            }
                            Pending::Nested => {
                                (Some(e), None)
                            }
                            Pending::Canceled => {
                                (None, Some(e))
                            }
                            _ => unreachable!(),
                        }
                    };

                    // TODO restructure this?
                    if let Some(e) = reenqueue {
                        let now = self.now();
                        self.enqueue_(e, now, now).err()
                    } else {
                        e
                    }
                } else {
                    Some(e)
                };

                // release memory?
                if let Some(e) = e { 
                    // call drop, return event to memory pool
                    self.dealloc_(e);
                }
            }

            // was break requested?
            if self.break_.load() {
                {   let guard = self.lock.lock();

                    if self.break_.load() {
                        self.break_.store(false);
                        return Dispatch::Break;
                    }
                }
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
            let delta = self.next_delta_(now)
                .into_iter().chain(timeout_left)
                .min();

            self.clock.wait(delta);

            now = self.now();
        }
    }

    pub fn dispatch<Δ: TryIntoDelta>(&self, delta: Option<Δ>) -> Dispatch {
        self.dispatch_(
            delta.map(|delta|
                delta.try_into_delta(self.clock.frequency()).ok()
                    .expect("delta overflow in equeue")
            )
        )
    }

    // request dispatch to exit once done dispatching events
    pub fn break_(&self) {
        {   let guard = self.lock.lock();
            self.break_.store(true);
        }

        self.clock.signal();
    }
}

impl<C: Clock+Sema> Equeue<C> {
    // Handling of raw allocations
    pub unsafe fn alloc_raw(&self, layout: Layout) -> *mut u8 {
        match self.alloc_(layout) {
            Ok(e) => e.data_mut_ptr(),
            Err(_) => ptr::null_mut(),
        }
    }

    pub unsafe fn dealloc_raw(&self, e: *mut u8, _layout: Layout) {
        debug_assert!(e.is_null() || self.contains_raw(e));
        let e = match Ebuf::from_data_mut_ptr(e) {
            Some(e) => e,
            None => return, // do nothing
        };

        // clean up ebuf
        self.dealloc_(e);
    }

    pub fn contains_raw(&self, e: *mut u8) -> bool {
        match unsafe { Ebuf::from_data_mut_ptr(e) } {
            Some(e) => self.contains(e),
            None => false,
        }
    }

    pub unsafe fn set_raw_delay<Δ: TryIntoDelta>(&self, e: *mut u8, delay: Δ) {
        debug_assert!(self.contains_raw(e));
        let e = Ebuf::from_data_mut_ptr(e).unwrap();
        e.target = delay.try_into_delta(self.clock.frequency()).ok()
            .expect("delta overflow in equeue")
            .as_tick();
    }

    pub unsafe fn set_raw_period<Δ: TryIntoDelta>(&self, e: *mut u8, period: Option<Δ>) {
        debug_assert!(self.contains_raw(e));
        let e = Ebuf::from_data_mut_ptr(e).unwrap();
        e.period = period.map(|period|
            period.try_into_delta(self.clock.frequency()).ok()
                .expect("delta overflow in equeue")
        );
    }

    pub unsafe fn set_raw_static(&self, e: *mut u8, static_: bool) {
        debug_assert!(self.contains_raw(e));
        let e = Ebuf::from_data_mut_ptr(e).unwrap();
        e.info.store(e.info.load().set_static(static_));
    }

    pub unsafe fn set_raw_drop(&self, e: *mut u8, drop: fn(*mut u8)) {
        debug_assert!(self.contains_raw(e));
        let e = Ebuf::from_data_mut_ptr(e).unwrap();
        e.drop = Some(drop);
    }

    pub unsafe fn post_raw(&self, cb: fn(*mut u8), e: *mut u8) -> Id {
        debug_assert!(self.contains_raw(e));
        let mut e = Ebuf::from_data_mut_ptr(e).unwrap();
        e.cb = Some(cb);

        let id = Id::new(self, e.info.load().id(), e);
        self.post_(e, e.target.as_delta().unwrap());
        id
    }
}


/// An id we can use to try to cancel an event
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Id {
    id: ugen,
    eptr: Eptr,
}

impl Id {
    pub const fn null() -> Id {
        // Ids can not be zero because that's where our buckets go in the slab
        Id {
            id: 0,
            eptr: Eptr::null(),
        }
    }

    fn new<C>(q: &Equeue<C>, id: ugen, e: &mut Ebuf) -> Id {
        Id {
            id: id,
            eptr: e.as_eptr(q),
        }
    }

    fn as_ref<'a, C>(&self, q: &'a Equeue<C>) -> Option<&'a Ebuf> {
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
pub struct Handle<'a, C=SysClock> {
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
    pub fn delta<Δ: TryFromDelta>(&self) -> Option<Δ> {
        self.q.delta(self.id)
    }
}

impl<'a, C: Clock+Sema> Handle<'a, C> {
    pub fn pend(&self) -> bool {
        self.q.pend(self.id)
    }
}

impl<C> Drop for Handle<'_, C> {
    fn drop(&mut self) {
        self.q.cancel(self.id);
    }
}


// TODO can we make Event accept ?Sized? The issue is not the allocation, 
// the issue is that we'd need to store a fat pointer somehow
/// Event handle
pub struct Event<'a, T, C=SysClock> {
    q: &'a Equeue<C>,
    e: &'a mut Ebuf,
    once: bool,
    _phantom: PhantomData<&'a mut T>,
}

impl<T: Debug, C> Debug for Event<'_, T, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Event")
            .field(&self.e)
            .field(&self.deref())
            .finish()
    }
}

impl<'a, T, C> Event<'a, T, C> {
    fn new(q: &'a Equeue<C>, e: &'a mut Ebuf, once: bool) -> Event<'a, T, C> {
        Event {
            q: q,
            e: e,
            once: once,
            _phantom: PhantomData
        }
    }
}

// event waker vtable callbacks
unsafe fn event_waker_clone<C: Clock+Sema>(e: *const ()) -> RawWaker {
    RawWaker::new(e, &Equeue::<C>::EVENT_WAKER_VTABLE)
}

unsafe fn event_waker_wake<C: Clock+Sema>(e: *const ()) {
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

    let mask = align_of::<Ebuf>()-1;
    let id_trunc = ((e as usize) & mask) as ugen;
    let e = Ebuf::from_data_mut_ptr(
        ((e as usize) & !mask) as *mut ()
    ).unwrap();

    // check that id matches first
    let id = e.info.load().id();
    if id_trunc != id & mask as ugen {
        return;
    }

    // pend
    let q = (e.q as *const Equeue<C>).as_ref().unwrap();
    q.pend(Id::new(q, id, e));
}

unsafe fn event_waker_drop(e: *const ()) {
    // do nothing
}

impl<C: Clock+Sema> Equeue<C> {
    const EVENT_WAKER_VTABLE: RawWakerVTable = RawWakerVTable::new(
        event_waker_clone::<C>,
        event_waker_wake::<C>,
        event_waker_wake::<C>,
        event_waker_drop,
    );
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

        Ok(Event::new(self, e, false))
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
            unsafe { Ebuf::from_data_mut_ptr(e) }.unwrap().drop = None;
            t.post_once();
        }

        fn drop_thunk<T: PostOnce>(e: *mut u8) {
            unsafe { drop_in_place(e as *mut T) };
        }

        e.cb = Some(cb_thunk::<T>);
        e.drop = Some(drop_thunk::<T>);

        // mark as a one-time event so we panic if we try to enqueue
        // multiple times
        Ok(Event::new(self, e, true))
    }

    pub fn alloc_static<'a, T: PostStatic<C> + Send>(&'a self, t: T) -> Result<Event<'a, T, C>, Error> {
        let e = self.alloc_(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(t); }
        e.q = self as *const Equeue<C> as *const Equeue;

        // cb/drop thunks
        fn cb_thunk<T: PostStatic<C>, C>(e: *mut u8) {
            let e = unsafe { Ebuf::from_data_mut_ptr(e) }.unwrap();
            let e = Event::new(unsafe { (e.q as *const Equeue<C>).as_ref() }.unwrap(), e, false);
            T::post_static(e);
        }

        fn drop_thunk<T>(e: *mut u8) {
            unsafe { drop_in_place(e as *mut T) };
        }

        e.cb = Some(cb_thunk::<T, C>);
        e.drop = Some(drop_thunk::<T>);

        // mark as static
        e.info.store(e.info.load().set_static(true));
        Ok(Event::new(self, e, false))
    }
}

impl<C: Clock+Sema> Equeue<C> {
    pub fn alloc_future<'a, T: Future<Output=()> + Send>(&'a self, t: T) -> Result<Event<'a, T, C>, Error> {
        let e = self.alloc_(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(t); }
        e.q = self as *const Equeue<C> as *const Equeue;

        fn cb_thunk<T: Future<Output=()>, C: Clock+Sema>(e: *mut u8) {
            let e = unsafe { Ebuf::from_data_mut_ptr(e) }.unwrap();
            debug_assert!(e.info.load().static_());
            let mut e = Event::<T>::new(unsafe { e.q.as_ref() }.unwrap(), e, false);

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
        Ok(Event::new(self, e, false))
    }
}

impl<'a, T, C: Clock> Event<'a, T, C> {
    pub fn delay<Δ: TryIntoDelta>(mut self, delay: Δ) -> Self {
        self.e.target = delay.try_into_delta(self.q.clock.frequency()).ok()
            .expect("delta overflow in equeue")
            .as_tick();
        self
    }

    pub fn period<Δ: TryIntoDelta>(mut self, period: Option<Δ>) -> Self {
        // can't set period for PostOnce events 
        assert!(!self.once);
        self.e.period = period.map(|period|
            period.try_into_delta(self.q.clock.frequency()).ok()
                .expect("delta overflow in equeue")
        );
        self
    }
}

impl <'a, T, C> Event<'a, T, C> {
    pub fn static_(self, static_: bool) -> Self {
        // can't make PostOnce events static
        assert!(!self.once);
        self.e.info.store(self.e.info.load().set_static(static_));
        self
    }

    // note this consumes the Event, otherwise we'd risk multiple access
    // if the id is pended while we still have a mutable reference
    pub fn into_id(self) -> Id {
        // mark as no longer in use, allowing external pends
        self.e.info.store(self.e.info.load().set_pending(Pending::Alloced));

        let id = Id::new(self.q, self.e.info.load().id(), self.e);
        forget(self);
        id
    }

    pub fn into_handle(self) -> Handle<'a, C> {
        Handle::new(self.q, self.into_id())
    }
}

impl<'a, T, C: Clock+Sema> Event<'a, T, C> {
    pub fn post(self) -> Id {
        // enqueue and then forget the event, it's up to equeue to
        // drop the event later
        let id = Id::new(self.q, self.e.info.load().id(), self.e);
        self.q.post_(self.e, self.e.target.as_delta().unwrap());
        forget(self);
        id
    }

    pub fn post_handle(self) -> Handle<'a, C> {
        Handle::new(self.q, self.post())
    }
}

impl<T, C> Drop for Event<'_, T, C> {
    fn drop(&mut self) {
        // clean up ebuf, note this runs the destructor internally
        self.q.dealloc_(self.e);
    }
}

impl<T, C> Deref for Event<'_, T, C> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { self.e.data_ref() }
    }
}

impl<T, C> DerefMut for Event<'_, T, C> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.e.data_mut() }
    }
}

impl<T, C> AsRef<T> for Event<'_, T, C> {
    fn as_ref(&self) -> &T {
        unsafe { self.e.data_ref() }
    }
}

impl<T, C> AsMut<T> for Event<'_, T, C> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { self.e.data_mut() }
    }
}


// convenience functions
impl<C: Clock+Sema> Equeue<C> {
    pub fn call<F: PostOnce + Send>(
        &self,
        cb: F
    ) -> Result<Id, Error> {
        Ok(
            self.alloc_once(cb)?
                .post()
        )
    }

    pub fn call_in<Δ: TryIntoDelta, F: PostOnce + Send>(
        &self,
        delay: Δ,
        cb: F
    ) -> Result<Id, Error> {
        Ok(
            self.alloc_once(cb)?
                .delay(delay)
                .post()
        )
    }

    pub fn call_every<Δ: TryIntoDelta, F: Post + Send>(
        &self,
        period: Δ,
        cb: F
    ) -> Result<Id, Error> {
        let period = period.try_into_delta(self.clock.frequency()).ok()
            .expect("delta overflow in equeue");
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

    pub fn call_in_handle<'a, Δ: TryIntoDelta, F: PostOnce + Send>(
        &'a self,
        delay: Δ,
        cb: F
    ) -> Result<Handle<'a, C>, Error> {
        self.call_in(delay, cb).map(|id| Handle::new(self, id))
    }

    pub fn call_every_handle<'a, Δ: TryIntoDelta, F: Post + Send>(
        &'a self,
        period: Δ,
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
struct AsyncYield<'a, C=SysClock> {
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
struct AsyncSleep<'a, C=SysClock> {
    q: &'a Equeue<C>,
    yielded: bool,
    timeout: Tick,
    id: Id,
}

impl<C: Clock+Sema> Future for AsyncSleep<'_, C> {
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
struct AsyncTimeout<'a, F, C=SysClock> {
    q: &'a Equeue<C>,
    f: F,
    timeout: Tick,
    id: Id,
}

impl<F: Future, C: Clock+Sema> Future for AsyncTimeout<'_, F, C> {
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

impl<C> Equeue<C> {
    pub async fn yield_(&self) {
        AsyncYield {
            q: self,
            yielded: false
        }.await
    }
}

impl<C: Clock+Sema> Equeue<C> {
    pub async fn sleep<Δ: TryIntoDelta>(&self, delta: Δ) -> Result<(), Error> {
        let delta = delta.try_into_delta(self.clock.frequency()).ok()
            .expect("delta overflow in equeue");
        AsyncSleep {
            q: self,
            yielded: false,
            timeout: self.now() + delta,
            id: Id::null(),
        }.await
    }

    pub async fn timeout<Δ: TryIntoDelta, F, R> (&self, delta: Δ, f: F) -> Result<R, Error>
    where
        F: Future<Output=R>
    {
        let delta = delta.try_into_delta(self.clock.frequency()).ok()
            .expect("delta overflow in equeue");
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
            // Note that we assume:
            // 1. dispatch does not block
            // 2. dispatch never enters self.clock.wait() 
            match self.dispatch(Some(Delta::zero())) {
                Dispatch::Timeout => {},
                Dispatch::Break => return Dispatch::Break,
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
            let delta = self.next_delta_(now)
                .into_iter().chain(timeout_left)
                .min();

            self.clock.wait_async(delta).await;
        }
    }

    pub async fn dispatch_async<Δ: TryIntoDelta>(&self, delta: Option<Δ>) -> Dispatch {
        self.dispatch_async_(
            delta.map(|delta|
                delta.try_into_delta(self.clock.frequency()).ok()
                    .expect("delta overflow in equeue")
            )
        ).await
    }
}


#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct Usage {
    pub pending: usize,
    pub pending_bytes: usize,
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
        let slab_front = self.slab_front.load() as usize;
        let slab_back = self.slab_back.load() as usize * align_of::<Ebuf>();
        let slab_unused = slab_back - slab_front;

        let mut total = 0usize;
        let mut i = slab_back;
        while let Some(e) = self.slab.get(i) {
            let e = unsafe { &*(e as *const _ as *const Ebuf) };

            total += 1;
            i = i.saturating_add(size_of::<Ebuf>() + e.size());
        }

        // find pending usage
        let mut pending = 0;
        let mut pending_bytes = 0;
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
                pending += 1;
                pending_bytes += size_of::<Ebuf>() + sibling.size();

                // this is all completely unsynchronized, so we have to set some
                // hard limits to prevent getting stuck in an infinite loop, 
                if pending > total {
                    pending = 1;
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
                free_bytes += size_of::<Ebuf>() + (align_of::<Ebuf>() << npw2);

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
        let pending = min(pending, total);
        let pending_bytes = min(pending_bytes, slab_total.saturating_sub(slab_unused+slab_front));
        let free = min(free, total-pending);
        let free_bytes = min(free_bytes, slab_total.saturating_sub(slab_unused+slab_front)-pending_bytes);

        Usage {
            pending: pending,
            pending_bytes: pending_bytes,
            alloced: total.saturating_sub(pending+free),
            alloced_bytes: slab_total.saturating_sub(pending_bytes+free_bytes+slab_unused+slab_front),
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
        let total = self.slab.len() / size_of::<Ebuf>();

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
        let total = self.slab.len() / size_of::<Ebuf>();

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



