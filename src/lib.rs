
// TODO enable
//#![no_std]

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
use core::cmp::Ordering;
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

mod util;
mod sys;
use util::*;
use sys::*;


/// Event queue errors
#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    NoMem,
    Overflow,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::NoMem => write!(f, "Out of memory"),
            Error::Overflow => write!(f, "Value could not fit in type"),
        }
    }
}


/// Small wrapper to generalize atomics to any <= udeptr sized type
#[repr(transparent)]
struct Atomic<T, S: AtomicU>(S, PhantomData<T>);

impl<T: Copy + Debug, S: AtomicU> Debug for Atomic<T, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Atomic")
            .field(&self.load())
            .finish()
    }
}

impl<T, S: AtomicU> Atomic<T, S> {
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

    fn from_ebuf(q: &Equeue, e: &Ebuf) -> Eptr {
        Eptr(unsafe {
            ((e as *const Ebuf as *const u8)
                .offset_from(q.slab.as_ptr())
                as usize
                / align_of::<Ebuf>())
                as ueptr
        })
    }

    fn as_ptr(&self, q: &Equeue) -> *const Ebuf {
        match self.0 {
            0 => ptr::null(),
            _ => (
                &q.slab[self.0 as usize * align_of::<Ebuf>()]
                    as *const _ as *const Ebuf
            )
        }
    }

    fn as_ref<'a>(&self, q: &'a Equeue) -> Option<&'a Ebuf> {
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

    fn from_ebuf(q: &Equeue, e: &mut Ebuf) -> MarkedEptr {
        MarkedEptr {
            gen: 0,
            eptr: Eptr::from_ebuf(q, e),
        }
    }

    fn as_ptr(&self, q: &Equeue) -> *const Ebuf {
        self.eptr.as_ptr(q)
    }

    fn as_ref<'a>(&self, q: &'a Equeue) -> Option<&'a Ebuf> {
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
impl<S: AtomicU> Atomic<MarkedEptr, S> {
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
    Alloced     = 0,
    Pending     = 2,
    Dispatching = 4,
    Nested      = 6,
    Canceled    = 7,
}

impl Pending {
    fn from_ueptr(pending: ueptr) -> Pending {
        match pending {
            0 | 1 => Pending::Alloced,
            2 | 3 => Pending::Pending,
            4 | 5 => Pending::Dispatching,
            6     => Pending::Nested,
            7     => Pending::Canceled,
            _ => unreachable!(),
        }
    }

    fn as_ueptr(&self) -> ueptr {
        match self {
            Pending::Alloced     => 0,
            Pending::Pending     => 2,
            Pending::Dispatching => 4,
            Pending::Nested      => 6,
            Pending::Canceled    => 7,
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
    target: utick,
    // TODO pack period and q into one field? they are mutually exclusive
    period: itick,
    q: *const Equeue,
}

impl Ebuf {
    fn as_eptr(&self, q: &Equeue) -> Eptr {
        Eptr::from_ebuf(q, self)
    }

    fn as_marked_eptr(&mut self, q: &Equeue) -> MarkedEptr {
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
    fn as_eptr(&self, q: &Equeue) -> Eptr;
}

impl<'a> OptionEbuf<'a> for Option<&'a Ebuf> {
    fn as_eptr(&self, q: &Equeue) -> Eptr {
        match self {
            Some(e) => e.as_eptr(q),
            None => Eptr::null(),
        }
    }
}


/// Event queue struct
#[derive(Debug)]
pub struct Equeue {
    // memory management
    slab: &'static [u8],
    slab_front: Atomic<usize, AtomicUsize>,
    slab_back: Atomic<usize, AtomicUsize>,

    // queue management
    queue: Atomic<MarkedEptr, AtomicUdeptr>,

    // other things
    clock: DefaultClock,
    lock: DefaultLock,
    sema: DefaultSema,
}

// TODO can we assert that these are already satisfied?
unsafe impl Send for Equeue {}
unsafe impl Sync for Equeue {}

impl Equeue {
    /// Number of bits of precision to use for scheduling events, this is limits
    /// the number of significant digits used in long-term events in order to
    /// create better bucketing and power consumption
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
    const PRECISION: u8 = 6;

    pub fn with_buffer(buffer: &'static mut [u8]) -> Result<Equeue, Error> {
        // align buffer
        let align = alignup(buffer.as_ptr() as usize, align_of::<Ebuf>())
            - buffer.as_ptr() as usize;
        let buffer = match buffer.get_mut(align..) {
            // already out of memory?
            Some(buffer) => buffer,
            None => return Err(Error::NoMem),
        };

        // go ahead and zero our buffer, this makes it easier to manage bucket
        // allocation, which needs to be null the moment a bucket is allocated
        buffer.fill(0);

        Ok(Equeue {
            slab: buffer,
            slab_front: Atomic::new(0),
            slab_back: Atomic::new(buffer.len()),

            queue: Atomic::new(MarkedEptr::null()),

            clock: DefaultClock::new(),
            lock: DefaultLock::new(),
            sema: DefaultSema::new(),
        })
    }

    pub fn now(&self) -> utick {
        self.clock.now()
    }

    fn imprecise_add(&self, a: utick, b: itick) -> utick {
        // In order to limit precision (which leads to more efficient data
        // structures and fewer wakeups/power consumption), we round up to
        // latest deadline in the configured precision. This just means
        // oring any bits lower than the precision with 1.
        //
        // Note that this always ensures a later deadline.
        let mask = (1 << (
            (8*size_of::<utick>() as u8).saturating_sub(
                b.leading_zeros() as u8 + Equeue::PRECISION
            )
        )) - 1;

        a.wrapping_add(b as utick) | mask
    }

    fn contains_ebuf(&self, e: &Ebuf) -> bool {
        self.slab.as_ptr_range()
            .contains(&(e.deref() as *const _ as *const u8))
    }

    fn buckets<'a>(&'a self) -> &'a [Atomic<Eptr, AtomicUeptr>] {
        let slab_front = self.slab_front.load() as usize;
        unsafe {
            slice::from_raw_parts(
                self.slab.as_ptr() as *const Atomic<Eptr, AtomicUeptr>,
                slab_front / size_of::<Eptr>()
            )
        }
    }

    // Memory management
    fn alloc_ebuf<'a>(&'a self, layout: Layout) -> Result<&'a mut Ebuf, Error> {
        // this looks arbitrary, but Ebuf should have a pretty reasonable
        // alignment since it contains both function pointers and AtomicUdeptrs
        assert!(layout.align() <= align_of::<Ebuf>());

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
            let e = self.lock.lock(|| {
                if let Some(e) = bucket.load().as_ref(self) {
                    bucket.store(e.sibling.load());
                    Some(e)
                } else {
                    None
                }
            });

            if let Some(e) = e {
                let mut e = unsafe { e.claim() };

                // zero certain fields
                e.cb = None;
                e.drop = None;
                e.target = 0;
                e.period = -1;
                e.q = ptr::null();
                return Ok(e);
            }
        }

        let new_slab_back = match self.lock.lock(|| {
            // check if we even have enough memory available, we allocate both
            // an event and maybe some buckets if we don't have enough
            let slab_front = self.slab_front.load();
            let slab_back = self.slab_back.load();
            let new_slab_front = max(
                (npw2 as usize + 1)*size_of::<Eptr>(),
                slab_front
            );
            let new_slab_back = aligndown(
                slab_back.saturating_sub(
                    size_of::<Ebuf>() + (align_of::<Ebuf>() << npw2)
                ),
                align_of::<Ebuf>()
            );

            if new_slab_front > new_slab_back {
                return Err(Error::NoMem);
            }

            // actually commit our allocation
            if new_slab_front > slab_front {
                self.slab_front.store(new_slab_front);
            }

            debug_assert!(new_slab_back < slab_back);
            self.slab_back.store(new_slab_back);

            Ok(new_slab_back)
        }) {
            Ok(new_slab_back) => new_slab_back,
            Err(err) => return Err(err),
        };

        unsafe {
            let e = &self.slab[new_slab_back as usize]
                as *const u8 as *const Ebuf as *mut Ebuf;
            e.write(Ebuf {
                next: Atomic::new(MarkedEptr::null()),
                next_back: Atomic::new(Eptr::null()),
                sibling: Atomic::new(Eptr::null()),
                sibling_back: Atomic::new(Eptr::null()),
                info: Atomic::new(Einfo::new(0, Pending::Alloced, false, npw2)),

                cb: None,
                drop: None,
                target: 0,
                period: -1,
                q: ptr::null(),
            });

            Ok(&mut *e)
        }
    }

    fn dealloc_ebuf(&self, e: &mut Ebuf) {
        debug_assert!(self.contains_ebuf(e));

        // we can load buckets here because it can never shrink
        let bucket = &self.buckets()[e.npw2() as usize];

        // add our event to a bucket, while also incrementing our
        // generation count
        self.lock.lock(|| {
            // give our event a new id
            let info = e.info.load();
            debug_assert_ne!(info.pending(), Pending::Pending);
            e.info.store(
                info.inc_id()
                    .set_pending(Pending::Alloced)
                    .set_static(false)
            );

            // push onto bucket
            let siblingptr = bucket.load();
            debug_assert_ne!(e as *const _, siblingptr.as_ptr(self));
            e.sibling.store(siblingptr);
            bucket.store(e.as_eptr(self));
        })
    }

    // Queue management
    fn enqueue_ebuf<'a>(
        &self,
        e: &'a mut Ebuf,
        target: utick,
        dispatching: bool
    ) -> Result<(), &'a mut Ebuf> {
        let mut e = e;
        debug_assert!(e.cb.is_some());
        e.target = target;

        'retry: loop {
            // find insertion point
            let mut back = None;
            let mut sibling = None;

            let mut headptr = self.queue.load();
            let mut headsrc = &self.queue;
            while let Some(head) = headptr.as_ref(self) {
                // compare targets
                match scmp(head.target, target) {
                    Ordering::Greater => {
                        sibling = None;
                        break;
                    }
                    Ordering::Equal => {
                        sibling = Some(head);
                        break;
                    }
                    Ordering::Less => {
                        // continue
                    }
                }

                back = Some(head);
                let nextptr = head.next.load();

                // check that the previous next pointer hasn't changed on us, if
                // so the node we were traversing could have been removed which
                // means we need to restart
                if headsrc.load() != headptr {
                    continue 'retry;
                }

                headptr = nextptr;
                headsrc = &head.next;
            }

            // prepare event before locking
            match sibling {
                None => {
                    // insert a new slice
                    e.next.store_marked(e.next.load(), headptr);
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
            match self.lock.lock(|| {
                // are we trying to enqueue an event that's already been canceled?
                //
                // this may seem seem like a weird place for this check, but
                // it's the only place we lock before re-enqueueing periodic events
                let info = e.info.load();
                match info.pending() {
                    Pending::Alloced => {},
                    Pending::Pending => return Some(true),
                    Pending::Dispatching | Pending::Nested if dispatching => {},
                    Pending::Dispatching => {
                        // we can't enqueue now, but we can mark as nested
                        e.info.store(info.set_pending(Pending::Nested));
                        return Some(true);
                    }
                    Pending::Nested => return Some(true),
                    Pending::Canceled => return None,
                }

                // did someone already change our headsrc? restart
                if headsrc.load() != headptr {
                    return Some(false);
                }

                // found our insertion point, now lets try to insert
                match sibling {
                    None => {
                        // insert a new slice
                        headsrc.store_marked(headptr, e.as_marked_eptr(self));
                        if let Some(next) = headptr.as_ref(self) {
                            next.next_back.store(e.as_eptr(self));
                        }
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
                    }
                }

                // mark as pending here, enabling removals
                e.info.store(info.set_pending(Pending::Pending));
                Some(true)
            }) {
                Some(true) => return Ok(()),
                Some(false) => continue 'retry,
                None => return Err(e),
            }
        }
    }

    // Central post function
    fn post(&self, e: &mut Ebuf, target: utick) {
        self.enqueue_ebuf(e, target, false).unwrap();

        // signal queue has changed
        self.sema.signal();
    }

    // repost an static event which may already be pending
    pub fn repost(&self, id: Id) {
        let e = id.as_ref(self);
        // TODO can we avoid this?
        let e = unsafe { e.claim() };
        
        // can't repost non-static events
        // TODO also check for id mismatches
        let info = e.info.load();
        if !info.static_() {
            return;
        }

        self.post(e, self.clock.now());
    }

    // Is the event pending?
    pub fn pending(&self, id: Id) -> bool {
        let e = id.as_ref(self);
        if !self.contains_ebuf(e) {
            return false;
        }

        // note that periodic events are always pending, we load period
        // first since it is not necessarily atomic
        let info = e.info.load();
        info.id() == id.id
            && match info.pending() {
                Pending::Alloced => false,
                Pending::Pending => true,
                Pending::Dispatching => info.static_() || e.period >= 0,
                Pending::Nested => true,
                Pending::Canceled => false,
            }
    }

    fn unqueue_ebuf<'a>(&'a self, id: Id) -> (bool, Option<&'a mut Ebuf>) {
        // check to see if we need to bother locking
        if !self.pending(id) {
            return (false, None);
        }

        let e = id.as_ref(self);

        self.lock.lock(|| {
            // still the same event?
            let info = e.info.load();
            if info.id() != id.id {
                return (false, None);
            }
            
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

                    if next_back.is_some() || headptr.as_ptr(self) == e as *const _ {
                        if sibling as *const _ == e as *const _ {
                            // just remove the slice

                            // update next_back's next/queue head first to avoid invalidating traversals
                            if headptr.as_ptr(self) == e as *const _ {
                                self.queue.store_marked(MarkedEptr::null(), nextptr);
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
                                self.queue.store_marked(MarkedEptr::null(), sibling.as_marked_eptr(self));
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
                    e.info.store(e.info.load().set_pending(Pending::Canceled));
                    
                    // note we are responsible for the memory now
                    let e = unsafe { e.claim() };

                    (true, Some(e))
                }
                Pending::Alloced => {
                    // well this shouldn't happen
                    unreachable!()
                }
                Pending::Dispatching | Pending::Nested => {
                    // if we're periodic/static and currently executing best we
                    // can do is mark the event so it isn't re-enqueued
                    e.info.store(info.set_pending(Pending::Canceled));
                    (info.static_() || e.period >= 0, None)
                }
                Pending::Canceled => {
                    (false, None)
                }
            }
        })
    }

    // Central cancel function
    pub fn cancel(&self, id: Id) -> bool {
        let (canceled, e) = self.unqueue_ebuf(id);

        if let Some(e) = e {
            // make sure to clean up memory
            if let Some(drop) = e.drop {
                drop(e.data_mut_ptr());
            }
            self.dealloc_ebuf(e);
        }

        canceled
    }

    // find time until next event without locking
    fn delta(&self, now: utick) -> Option<itick> {
        'retry: loop {
            let headptr = self.queue.load();
            let target = match headptr.as_ref(self) {
                Some(head) => head.target,
                None => return None,
            };

            // make sure headptr hasn't changed before we trust our target
            if self.queue.load() != headptr {
                continue 'retry;
            }

            return Some(sdiff(target, now));
        }
    }

    fn dequeue_ebufs<'a>(&'a self, now: utick) -> Option<&'a Ebuf> {
        // after several implementations, this turned out rather simple,
        // we just need to lock, remove the head of each slice, and make sure
        // all back-references/generation counts are correct
        //
        // note we grab every slice that is available to run, which may
        // be several
        let mut dequeued = None;

        loop {
            // check if we need to bother locking first
            if self.delta(now).filter(|&delta| delta <= 0).is_none() {
                break;
            }

            if !self.lock.lock(|| {
                match self.queue.load().as_ref(self) {
                    // is this slice ready to dispatch?
                    Some(head) if scmp(head.target, now).is_le() => {
                        // remove slice from queue
                        let nextptr = head.next.load();
                        // make sure we update queue first to not get any readers stuck
                        self.queue.store(nextptr);
                        head.next.store_inc_marked(nextptr, MarkedEptr::null());
                        head.next_back.store(Eptr::null());

                        // make sure back-references are updated
                        if let Some(next) = nextptr.as_ref(self) {
                            next.next_back.store(Eptr::null());
                        }

                        match dequeued {
                            None => {
                                // before we unlock, head is in a weird state where if it's
                                // canceled we're suddenly pointing to garbage, so we go ahead 
                                // and mark the head as executing, since that's the very next
                                // thing we're going to do
                                head.info.store(head.info.load().set_pending(Pending::Dispatching));
                                dequeued = Some(head);
                            }
                            Some(dequeued) => {
                                // append slice to what's already been dequeued
                                let dequeued_back = dequeued.sibling_back.load()
                                    .as_ref(self).unwrap();
                                let head_back = head.sibling_back.load()
                                    .as_ref(self).unwrap();

                                dequeued_back.sibling.store(head.as_eptr(self));
                                head.sibling_back.store(dequeued_back.as_eptr(self));

                                head_back.sibling.store(dequeued.as_eptr(self));
                                dequeued.sibling_back.store(head_back.as_eptr(self));
                            }
                        }
                        true
                    }
                    _ => {
                        false
                    }
                }
            }) {
                break;
            }
        }

        dequeued
    }

    // Central dispatch function
    pub fn dispatch(&self, ticks: itick) {
        // get the current time
        let mut now = self.clock.now();
        let timeout = now.wrapping_add(ticks as utick);

        loop {
            // get a slice to dispatch
            let mut slice = self.dequeue_ebufs(now);

            while let Some(e) = slice {
                // last chance to cancel
                self.lock.lock(|| {
                    // remove from sibling list
                    let sibling = e.sibling.load().as_ref(self).unwrap();
                    let sibling_back = e.sibling_back.load().as_ref(self).unwrap();
                    if sibling as *const _ == e as *const _ {
                        slice = None;
                    } else {
                        // mark the next event as executing
                        sibling.info.store(sibling.info.load().set_pending(Pending::Dispatching));

                        sibling.sibling_back.store(sibling_back.as_eptr(self));
                        sibling_back.sibling.store(sibling.as_eptr(self));
                        slice = Some(sibling);
                    }
                });

                // we now have exclusive access
                let e = unsafe { e.claim() };

                // load id here so we can tell if it changes (which happens if the
                // event is reclaimed in dispatch)
                let id = e.info.load().id();

                // dispatch!
                (e.cb.unwrap())(e.data_mut_ptr());

                let info = e.info.load();
                let e = if id != info.id() {
                    None
                } else if info.static_() {
                    // if static, try to mark as no-longer pending, but
                    // note we could be canceled or recursively pended
                    let (reenqueue, e) = self.lock.lock(|| {
                        let info = e.info.load();
                        match info.pending() {
                            Pending::Dispatching => {
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
                    });

                    if let Some(e) = reenqueue {
                        self.enqueue_ebuf(e, self.clock.now(), true).err()
                    } else {
                        e
                    }
                } else if e.period >= 0 {
                    // if periodic, reenqueue, unless we get canceled
                    self.enqueue_ebuf(
                        e,
                        self.imprecise_add(self.clock.now(), e.period),
                        true
                    ).err()
                } else {
                    Some(e)
                };

                // release memory?
                if let Some(e) = e { 
                    // call drop, return event to memory pool
                    if let Some(drop) = e.drop {
                        drop(e.data_mut_ptr());
                    }
                    self.dealloc_ebuf(e);
                }
            }

            // should we stop dispatching?
            //
            // note that time could have changed _significantly_
            now = self.clock.now();
            let timeout_left = sdiff(timeout, now);
            if ticks >= 0 && timeout_left <= 0 {
                return;
            }

            // ok how long should we sleep for
            //
            // Note that we always try to sleep between slices, this is
            // just to behave nicely in case the system's semaphore implementation
            // does something "clever". Note we also never enter here if
            // ticks is 0 for similar reasons.
            let mut delta = match self.delta(now) {
                Some(delta) => max(delta, 0),
                None => -1,
            };

            if (delta as utick) > (timeout_left as utick) {
                delta = timeout_left;
            }

            self.sema.wait(delta);

            // update current time
            now = self.clock.now();
        }
    }
}

impl Equeue {
    // Handling of raw allocations
    pub unsafe fn alloc_raw(&self, layout: Layout) -> *mut u8 {
        match self.alloc_ebuf(layout) {
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

        // make sure we call drop!
        if let Some(drop) = e.drop {
            drop(e.data_mut_ptr());
        }

        // if we don't post, we need to increment our generation here
        self.dealloc_ebuf(e);
    }

    pub fn contains_raw(&self, e: *mut u8) -> bool {
        match unsafe { Ebuf::from_data_mut_ptr(e) } {
            Some(e) => self.contains_ebuf(e),
            None => false,
        }
    }

    pub unsafe fn set_raw_delay(&self, e: *mut u8, delay: itick) {
        debug_assert!(self.contains_raw(e));
        let e = Ebuf::from_data_mut_ptr(e).unwrap();
        debug_assert!(delay >= 0);
        e.target = delay as utick;
    }

    pub unsafe fn set_raw_period(&self, e: *mut u8, period: itick) {
        debug_assert!(self.contains_raw(e));
        let e = Ebuf::from_data_mut_ptr(e).unwrap();
        e.period = period;
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

        let id = Id::new(self, e);
        self.post(e, self.imprecise_add(self.clock.now(), e.target as itick));
        id
    }
}


/// Post trait
pub trait Post: Send {
    fn post(&mut self);
}

impl<F: FnMut() + Send> Post for F {
    fn post(&mut self) {
        self()
    }
}

/// Post-once trait, a special case
pub trait PostOnce: Send {
    fn post_once(self);
}

impl<F: FnOnce() + Send> PostOnce for F {
    fn post_once(self) {
        self()
    }
}

/// Post-static trait, a post that does not reclaim memory
pub trait PostStatic: Sized + Send {
    fn post_static(self_: Event<'_, Self>);
}


/// An id we can use to try to cancel an event
#[derive(Copy, Clone)]
pub struct Id {
    id: ugen,
    eptr: NonZeroUeptr,
}

impl Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // these really need to be in hex to be readable
        f.debug_tuple("Id")
            .field(&self.id)
            .field(&format_args!("{:#x}", self.eptr))
            .finish()
    }
}

impl Id {
    fn new(q: &Equeue, e: &mut Ebuf) -> Id {
        Id {
            id: e.info.load().id(),
            eptr: unsafe { NonZeroUeptr::new_unchecked(e.as_eptr(q).0) },
        }
    }

    fn as_ref<'a>(&self, q: &'a Equeue) -> &'a Ebuf {
        Eptr(self.eptr.get()).as_ref(q).unwrap()
    }
}


/// Event handle
pub struct Event<'a, T> {
    q: &'a Equeue,
    e: &'a mut Ebuf,
    once: bool,
    _phantom: PhantomData<T>,
}

impl<T: Debug> Debug for Event<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Event")
            .field(&self.e)
            .field(&self.deref())
            .finish()
    }
}

impl<'a, T> Event<'a, T> {
    fn new(q: &'a Equeue, e: &'a mut Ebuf, once: bool) -> Event<'a, T> {
        Event {
            q: q,
            e: e,
            once: once,
            _phantom: PhantomData
        }
    }
}

// event waker vtable callbacks
unsafe fn event_waker_clone(e: *const ()) -> RawWaker {
    RawWaker::new(e, &EVENT_WAKER_VTABLE)
}

unsafe fn event_waker_wake(e: *const ()) {
    let e = Ebuf::from_data_mut_ptr(e as *mut ()).unwrap();
    let q = e.q.as_ref().unwrap();
    q.post(e, q.now());
}

unsafe fn event_waker_drop(e: *const ()) {
    // do nothing
}

const EVENT_WAKER_VTABLE: RawWakerVTable = RawWakerVTable::new(
    event_waker_clone,
    event_waker_wake,
    event_waker_wake,
    event_waker_drop,
);

impl Equeue {
    pub fn alloc<'a, T: Post>(&'a self, t: T) -> Result<Event<'a, T>, Error> {
        let e = self.alloc_ebuf(Layout::new::<T>())?;
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

    pub fn alloc_once<'a, T: PostOnce>(&'a self, t: T) -> Result<Event<'a, T>, Error> {
        let e = self.alloc_ebuf(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(t); }

        // cb/drop thunks
        fn cb_thunk<T: PostOnce>(e: *mut u8) {
            unsafe { (e as *mut T).read() }.post_once();
        }

        e.cb = Some(cb_thunk::<T>);
        e.drop = None; // e will already be dropped

        // mark as a one-time event so we panic if we try to enqueue
        // multiple times
        Ok(Event::new(self, e, true))
    }

    pub fn alloc_static<'a, T: PostStatic>(&'a self, t: T) -> Result<Event<'a, T>, Error> {
        let e = self.alloc_ebuf(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(t); }
        e.q = self as *const Equeue;

        // cb/drop thunks
        fn cb_thunk<T: PostStatic>(e: *mut u8) {
            let e = unsafe { Ebuf::from_data_mut_ptr(e) }.unwrap();
            let e = Event::new(unsafe { e.q.as_ref() }.unwrap(), e, false);
            T::post_static(e);
        }

        fn drop_thunk<T>(e: *mut u8) {
            unsafe { drop_in_place(e as *mut T) };
        }

        e.cb = Some(cb_thunk::<T>);
        e.drop = Some(drop_thunk::<T>);

        // mark as static
        e.info.store(e.info.load().set_static(true));
        Ok(Event::new(self, e, false))
    }

    pub fn alloc_future<'a, T: Future<Output=()>>(&'a self, t: T) -> Result<Event<'a, T>, Error> {
        let e = self.alloc_ebuf(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(t); }
        e.q = self as *const Equeue;

        fn cb_thunk<T: Future<Output=()>>(e: *mut u8) {
            let e = unsafe { Ebuf::from_data_mut_ptr(e) }.unwrap();
            assert!(e.info.load().static_());
            let mut e = Event::<T>::new(unsafe { e.q.as_ref() }.unwrap(), e, false);

            let waker = unsafe {
                Waker::from_raw(event_waker_clone(
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

        e.cb = Some(cb_thunk::<T>);
        e.drop = Some(drop_thunk::<T>);

        // mark as static
        e.info.store(e.info.load().set_static(true));
        Ok(Event::new(self, e, false))
    }
}

impl<T> Event<'_, T> {
    pub fn delay(mut self, delay: itick) -> Self {
        debug_assert!(delay >= 0);
        self.e.target = delay as utick;
        self
    }

    pub fn period(mut self, period: itick) -> Self {
        // can't set period for PostOnce events 
        assert!(!self.once);
        self.e.period = period;
        self
    }

    // note this consumes the Event, otherwise we'd risk multiple access
    // if the id is reposted
    pub fn into_id(self) -> Id {
        let id = Id::new(self.q, self.e);
        forget(self);
        id
    }

    pub fn post(self) -> Id {
        // enqueue and then forget the event, it's up to equeue to
        // drop the event later
        let id = Id::new(self.q, self.e);
        self.q.post(self.e, self.q.imprecise_add(self.q.now(), self.e.target as itick));
        forget(self);
        id
    }
}

// TODO we also need to implement drop for Equeue
impl<T> Drop for Event<'_, T> {
    fn drop(&mut self) {
        // make sure we clean up if the event isn't dispatched
        unsafe { drop_in_place(self.e.data_mut_ptr::<T>()) };

        // have to work around our own Emut lifetime here
        self.q.dealloc_ebuf(self.e);
    }
}

impl<T> Deref for Event<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { self.e.data_ref() }
    }
}

impl<T> DerefMut for Event<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.e.data_mut() }
    }
}

impl<T> AsRef<T> for Event<'_, T> {
    fn as_ref(&self) -> &T {
        unsafe { self.e.data_ref() }
    }
}

impl<T> AsMut<T> for Event<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { self.e.data_mut() }
    }
}


// convenience functions
impl Equeue {
    pub fn call<F: PostOnce>(&self, cb: F) -> Result<Id, Error>{
        Ok(
            self.alloc_once(cb)?
                .post()
        )
    }

    pub fn call_in<F: PostOnce>(&self, delay: itick, cb: F) -> Result<Id, Error> {
        Ok(
            self.alloc_once(cb)?
                .delay(delay)
                .post()
        )
    }

    pub fn call_every<F: Post>(&self, period: itick, cb: F) -> Result<Id, Error> {
        Ok(
            self.alloc(cb)?
                .delay(period)
                .period(period)
                .post()
        )
    }

    pub fn run<F: Future<Output=()>>(&self, cb: F) -> Result<Id, Error> {
        Ok(
            self.alloc_future(cb)?
                .post()
        )
    }
}


#[derive(Debug, Clone)]
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

impl Equeue {
    pub fn usage(&self) -> Usage {
        // find slab usage
        let slab_total = self.slab.len();
        let slab_front = self.slab_front.load() as usize;
        let slab_back = self.slab_back.load() as usize;
        let slab_unused = slab_back - slab_front;

        let mut total = 0usize;
        let mut p = self.slab.get(slab_back)
            .map(|p| p as *const u8)
            .unwrap_or(ptr::null());
        while self.slab.as_ptr_range().contains(&p) {
            let e = unsafe { &*(p as *const Ebuf) };

            total += 1;
            p = p.wrapping_add(size_of::<Ebuf>() + e.size());
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



