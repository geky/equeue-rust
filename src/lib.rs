
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


/// Small wrapper to generalize atomics to any <= uatom sized type
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
                / Ebuf::ALIGN)
                as ueptr
        })
    }

    fn as_ptr(&self, q: &Equeue) -> *const Ebuf {
        match self.0 {
            0 => ptr::null(),
            _ => (
                &q.slab[self.0 as usize * Ebuf::ALIGN]
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
    mark: ugen,
    gen: ugen,
    eptr: Eptr,
}

impl Debug for MarkedEptr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // these really need to be in hex to be readable
        f.debug_tuple("MarkedEptr")
            .field(&self.mark)
            .field(&self.gen)
            .field(&format_args!("{:#x}", self.eptr.0))
            .finish()
    }
}

impl MarkedEptr {
    const fn null() -> MarkedEptr {
        MarkedEptr {
            mark: 0,
            gen: 0,
            eptr: Eptr::null(),
        }
    }

    // avoids loading the generation count from the ebuf
    fn from_parts(q: &Equeue, mark: ugen, gen: ugen, e: &Ebuf) -> MarkedEptr {
        MarkedEptr {
            mark: mark,
            gen: gen,
            eptr: Eptr::from_ebuf(q, e),
        }
    }

    // loads gen from the ebuf, the resulting MarkedEptr will always be in sync
    //
    // we take a mutable ebuf to avoid races, otherwise we probably don't have
    // exclusive access, so the code would probably be broken anyways
    fn from_ebuf(q: &Equeue, mark: ugen, e: &mut Ebuf) -> MarkedEptr {
        MarkedEptr {
            mark: mark,
            gen: e.next.load().mark,
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
    fn check_mark(self, src: MarkedEptr) -> Result<MarkedEptr, MarkedEptr> {
        if src.gen == self.mark {
            Ok(self)
        } else {
            Err(self)
        }
    }

    fn cp_mark(self, src: MarkedEptr) -> MarkedEptr {
        MarkedEptr {
            mark: src.mark,
            gen: self.gen,
            eptr: self.eptr,
        }
    }

    fn inc_mark(self) -> MarkedEptr {
        MarkedEptr {
            mark: self.mark.wrapping_add(1),
            gen: self.gen,
            eptr: self.eptr,
        }
    }
}

// interactions with atomics
impl<S: AtomicU> Atomic<MarkedEptr, S> {
    fn load_marked(&self, src: MarkedEptr) -> Result<MarkedEptr, MarkedEptr> {
        self.load().check_mark(src)
    }

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

impl Einfo {
    fn new(id: ugen, pending: bool, canceled: bool, npw2: u8) -> Einfo {
        Einfo(
            ((id as ueptr) << 8*size_of::<ugen>())
            | ((pending as ueptr) << (8*size_of::<ugen>()-1))
            | ((canceled as ueptr) << (8*size_of::<ugen>()-2))
            | (npw2 as ueptr)
        )
    }

    fn id(&self) -> ugen {
        (self.0 >> 8*size_of::<ugen>()) as ugen
    }

    fn pending(&self) -> bool {
        self.0 & (1 << (8*size_of::<ugen>()-1)) != 0
    }

    fn canceled(&self) -> bool {
        self.0 & (1 << (8*size_of::<ugen>()-2)) != 0
    }

    fn npw2(&self) -> u8 {
        (self.0 & ((1 << (8*size_of::<ugen>()-2)) - 1)) as u8
    }

    fn inc_id(self) -> Einfo {
        Einfo(self.0.wrapping_add(1 << 8*size_of::<ugen>()))
    }

    fn set_pending(self, pending: bool) -> Einfo {
        if pending {
            Einfo(self.0 | (1 << (8*size_of::<ugen>()-1)))
        } else {
            Einfo(self.0 & !(1 << (8*size_of::<ugen>()-1)))
        }
    }

    fn set_canceled(self, canceled: bool) -> Einfo {
        if canceled {
            Einfo(self.0 | (1 << (8*size_of::<ugen>()-2)))
        } else {
            Einfo(self.0 & !(1 << (8*size_of::<ugen>()-2)))
        }
    }
}

/// Internal event header
#[derive(Debug)]
struct Ebuf {
    next: Atomic<MarkedEptr, AtomicUatom>,
    next_back: Atomic<Eptr, AtomicUeptr>,
    sibling: Atomic<Eptr, AtomicUeptr>,
    sibling_back: Atomic<Eptr, AtomicUeptr>,
    info: Atomic<Einfo, AtomicUeptr>,

    cb: Option<fn(*mut u8)>,
    drop: Option<fn(*mut u8)>,
    target: utick,
    period: itick,
}

impl Ebuf {
    // maximum alignment of internal allocations, this must be larger than
    // Eptr's alignment, and pointer alignment is a common alignment
    //
    // TODO should these take advantage of the npw2(event size) to be compressed
    // even further?
    //
    const ALIGN: usize = {
        let mut align = align_of::<Ebuf>();
        if align_of::<Eptr>() > align { align = align_of::<Eptr>() }
        if align_of::<*const ()>() > align { align = align_of::<*const ()>() }
        align
    };

    fn as_eptr(&self, q: &Equeue) -> Eptr {
        Eptr::from_ebuf(q, self)
    }

    fn as_marked_eptr(&mut self, q: &Equeue) -> MarkedEptr {
        MarkedEptr::from_ebuf(q, 0, self)
    }

    // info access
    fn npw2(&self) -> u8 {
        self.info.load().npw2()
    }

    fn size(&self) -> usize {
        Ebuf::ALIGN << self.npw2()
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
    queue: Atomic<MarkedEptr, AtomicUatom>,

    // other things
    clock: DefaultClock,
    lock: DefaultLock,
    sema: DefaultSema,
}

// TODO can we assert that these are already satisfied?
unsafe impl Send for Equeue {}
unsafe impl Sync for Equeue {}

impl Equeue {
    pub fn with_buffer(buffer: &'static mut [u8]) -> Result<Equeue, Error> {
        // align buffer
        let align = alignup(buffer.as_ptr() as usize, Ebuf::ALIGN)
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
        assert!(layout.align() <= Ebuf::ALIGN);

        // find best bucket
        let npw2 = npw2((layout.size()+Ebuf::ALIGN-1) / Ebuf::ALIGN);

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
                return Ok(e);
            }
        }

        let new_slab_back = loop {
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
                    size_of::<Ebuf>() + (Ebuf::ALIGN << npw2)
                ),
                Ebuf::ALIGN
            );

            if new_slab_front > new_slab_back {
                return Err(Error::NoMem);
            }

            // try to actually commit our allocation, if slab_front/slab_back
            // changed already, we just try again, someone should be making
            // progress if that happens
            if self.lock.lock(|| {
                if self.slab_front.load() == slab_front
                    && self.slab_back.load() == slab_back
                {
                    if new_slab_front > slab_front {
                        self.slab_front.store(new_slab_front);
                    }

                    debug_assert!(new_slab_back < slab_back);
                    self.slab_back.store(new_slab_back);

                    true
                } else {
                    false
                }
            }) {
                break new_slab_back;
            }
        };

        unsafe {
            let e = &self.slab[new_slab_back as usize]
                as *const u8 as *const Ebuf as *mut Ebuf;
            e.write(Ebuf {
                next: Atomic::new(MarkedEptr::null()),
                next_back: Atomic::new(Eptr::null()),
                sibling: Atomic::new(Eptr::null()),
                sibling_back: Atomic::new(Eptr::null()),
                info: Atomic::new(Einfo::new(0, false, false, npw2)),

                cb: None,
                drop: None,
                target: 0,
                period: -1,
            });

            Ok(&mut *e)
        }
    }

    fn dealloc_ebuf(&self, e: &mut Ebuf) {
        debug_assert!(self.contains_ebuf(e));

        // we can load buckets here because it can never shrink
        let bucket = &self.buckets()[e.npw2() as usize];

        // give our event a new id
        e.info.store(
            e.info.load()
                .inc_id()
                .set_pending(false)
                .set_canceled(false)
        );

        // add our event to a bucket, while also incrementing our
        // generation count
        self.lock.lock(|| {
            // push onto bucket
            let siblingptr = bucket.load();
            debug_assert_ne!(e as *const _, siblingptr.as_ptr(self));
            e.sibling.store(siblingptr);
            bucket.store(e.as_eptr(self));
        })
    }

    // Queue management
    fn enqueue_ebuf<'a>(&self, e: &'a mut Ebuf, target: utick) -> Result<Id, &'a mut Ebuf> {
        let mut e = e;
        debug_assert!(e.cb.is_some());
        e.target = target;

        let id = Id::from_ebuf(self, e);

        'retry: loop {
            // find insertion point
            let mut back = None;
            let mut sibling = None;

            let mut headsrc = &self.queue;
            let mut headptr = self.queue.load();
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
                headsrc = &head.next;
                headptr = match head.next.load_marked(headptr) {
                    Ok(headptr) => headptr,
                    Err(_) => {
                        // found a gen mismatch, most likely some changed
                        // happened to the data-structure which puts us into
                        // an unknown state, so we need to restart
                        continue 'retry;
                    }
                };
            }

            // found our insertion point, now lets try to insert
            match sibling {
                None => {
                    // insert a new slice
                    e.next.store_marked(e.next.load(), headptr);
                    e.next_back.store(back.as_eptr(self));
                    e.sibling.store(e.as_eptr(self));
                    e.sibling_back.store(e.as_eptr(self));

                    // try to insert
                    match self.lock.lock(|| {
                        // this may seem seem like a weird place for this check, but
                        // it's the only place we lock before re-enqueueing periodic events
                        if e.info.load().canceled() {
                            None
                        } else if headsrc.load() == headptr {
                            headsrc.store_marked(headptr, e.as_marked_eptr(self));
                            if let Some(next) = headptr.as_ref(self) {
                                next.next_back.store(e.as_eptr(self));
                            }

                            // mark as pending here, enabling removals
                            e.info.store(
                                e.info.load()
                                    .set_pending(true)
                                    .set_canceled(false)
                            );
                            Some(true)
                        } else {
                            // did someone already change our headsrc? restart
                            Some(false)
                        }
                    }) {
                        Some(true) => return Ok(id),
                        Some(false) => continue 'retry,
                        None => return Err(e),
                    }
                }
                Some(sibling) => {
                    // push onto existing slice
                    e.next.store_marked(e.next.load(), MarkedEptr::null());
                    e.next_back.store(Eptr::null());
                    e.sibling.store(sibling.as_eptr(self));

                    // try to push
                    match self.lock.lock(|| {
                        // this may seem seem like a weird place for this check, but
                        // it's the only place we lock before re-enqueueing periodic events
                        if e.info.load().canceled() {
                            None
                        } else if headsrc.load() == headptr {
                            // the real need for locking here is to make sure all of
                            // the back-references are correct atomically
                            let sibling_back = sibling.sibling_back.load()
                                .as_ref(self).unwrap();
                            sibling.sibling_back.store(e.as_eptr(self));
                            
                            sibling_back.sibling.store(e.as_eptr(self));
                            e.sibling_back.store(sibling_back.as_eptr(self));

                            // mark as pending here, enabling removals
                            e.info.store(
                                e.info.load()
                                    .set_pending(true)
                                    .set_canceled(false)
                            );
                            Some(true)
                        } else {
                            // did someone already change our headsrc? restart
                            Some(false)
                        }
                    }) {
                        Some(true) => return Ok(id),
                        Some(false) => continue 'retry,
                        None => return Err(e),
                    }
                }
            }
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
                                head.info.store(head.info.load().set_pending(false));
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
            if info.pending() {
                // we can disentangle the event here and reclaim the memory
                let nextptr = e.next.load();
                let next = nextptr.as_ref(self);
                let next_back = e.next_back.load().as_ref(self);
                let sibling = e.sibling.load().as_ref(self).unwrap();
                let sibling_back = e.sibling_back.load().as_ref(self).unwrap();

                // we also need to remove from the queue if we are the head, and
                // this needs to be done first to avoid invalidating traversals.
                // we can't just point to queue here with next_back because eptrs
                // are limited to in-slab events.
                if self.queue.load().as_ptr(self) == e as *const _ {
                    self.queue.store_marked(MarkedEptr::null(), nextptr);
                }

                // update next_back's next first to avoid invalidating traversals
                if let Some(next_back) = next_back {
                    next_back.next.store_marked(next_back.next.load(), nextptr);
                }
                if let Some(next) = next {
                    next.next_back.store(next_back.as_eptr(self));
                }
                sibling_back.sibling.store(sibling.as_eptr(self));
                sibling.sibling_back.store(sibling_back.as_eptr(self));

                // mark as removed
                e.next.store_inc_marked(nextptr, MarkedEptr::null());
                // mark as not-pending
                e.info.store(e.info.load().set_pending(false));
                
                // note we are responsible for the memory now
                let e = unsafe { e.claim() };

                (true, Some(e))
            } else if e.period >= 0 {
                // if we're periodic and currently executing best we can do is
                // mark the event so it isn't re-enqueued
                e.info.store(info.set_canceled(true));
                (true, None)
            } else {
                (false, None)
            }
        })
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

    // Central post function
    fn post(&self, e: &mut Ebuf, target: utick) -> Id {
        let id = self.enqueue_ebuf(e, target).unwrap();

        // signal queue has changed
        self.sema.signal();

        id
    }

    // Central dispatch function
    pub fn dispatch(&self, ticks: itick) {
        // get the current time
        let mut now = self.clock.now();
        let timeout = now.wrapping_add(ticks as u64);

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
                        sibling.info.store(sibling.info.load().set_pending(false));

                        sibling.sibling_back.store(sibling_back.as_eptr(self));
                        sibling_back.sibling.store(sibling.as_eptr(self));
                        slice = Some(sibling);
                    }
                });

                // we now have exclusive access
                let e = unsafe { e.claim() };

                // dispatch!
                e.cb.unwrap()(e.data_mut_ptr());

                let e = if e.period >= 0 {
                    // reenqueue?
                    self.enqueue_ebuf(
                        e,
                        self.clock.now().wrapping_add(e.period as u64)
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

    // Is the event pending?
    pub fn pending(&self, id: Id) -> bool {
        let e = id.as_ref(self);
        if !self.contains_ebuf(e) {
            return false;
        }

        // note that periodic events are always pending, we load period
        // first since it is not necessarily atomic
        let period = e.period;
        let info = e.info.load();
        info.id() == id.id && (info.pending() || period >= 0)
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

    pub unsafe fn set_raw_drop(&self, e: *mut u8, drop: fn(*mut u8)) {
        debug_assert!(self.contains_raw(e));
        let e = Ebuf::from_data_mut_ptr(e).unwrap();
        e.drop = Some(drop);
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

    pub unsafe fn post_raw(&self, cb: fn(*mut u8), e: *mut u8) -> Id {
        debug_assert!(self.contains_raw(e));
        let mut e = Ebuf::from_data_mut_ptr(e).unwrap();
        e.cb = Some(cb);
        self.post(e, self.clock.now().wrapping_add(e.target))
    }
}


/// Post trait
pub trait Post {
    fn post(&mut self);
}

// TODO wait, should we also accept FnOnce for non-periodic events?
impl<F: FnMut() + Send> Post for F {
    fn post(&mut self) {
        self()
    }
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
    fn from_ebuf(q: &Equeue, e: &mut Ebuf) -> Id {
        Id {
            id: e.info.load().id(),
            eptr: unsafe { NonZeroUeptr::new_unchecked(e.as_eptr(q).0) }
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
    fn new(q: &'a Equeue, e: &'a mut Ebuf) -> Event<'a, T> {
        Event {
            q: q,
            e: e,
            _phantom: PhantomData
        }
    }

    pub unsafe fn from_raw_parts(q: &'a Equeue, e: *mut T) -> Event<'a, T> {
        debug_assert!(q.contains_raw(e as *mut u8));
        let e = Ebuf::from_data_mut_ptr(e).unwrap();
        Event::new(q, e)
    }
}

impl Equeue {
    pub fn alloc<'a, T: Default>(&'a self) -> Result<Event<'a, T>, Error> {
        let e = self.alloc_ebuf(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(T::default()); }
        Ok(Event::new(self, e))
    }

    pub fn alloc_from<'a, T>(&'a self, t: T) -> Result<Event<'a, T>, Error> {
        let e = self.alloc_ebuf(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(t); }
        Ok(Event::new(self, e))
    }
}

impl<T> Event<'_, T> {
    pub fn delay(mut self, delay: itick) -> Self {
        debug_assert!(delay >= 0);
        self.e.target = delay as utick;
        self
    }

    pub fn period(mut self, period: itick) -> Self {
        self.e.period = period;
        self
    }
}

impl<T: Post> Event<'_, T> {
    pub fn post(self) -> Id {
        // cb/drop thunks
        fn cb_thunk<T: Post>(e: *mut u8) {
            unsafe { &mut *(e as *mut T) }.post();
        }

        fn drop_thunk<T>(e: *mut u8) {
            unsafe { drop_in_place(e as *mut T) };
        }

        self.e.cb = Some(cb_thunk::<T>);
        self.e.drop = Some(drop_thunk::<T>);

        // enqueue and then forget the event, it's up to equeue to
        // drop the event later
        let id = self.q.post(self.e, self.q.now().wrapping_add(self.e.target));
        forget(self);

        id
    }
}

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

impl<T> Borrow<T> for Event<'_, T> {
    fn borrow(&self) -> &T {
        unsafe { self.e.data_ref() }
    }
}

impl<T> BorrowMut<T> for Event<'_, T> {
    fn borrow_mut(&mut self) -> &mut T {
        unsafe { self.e.data_mut() }
    }
}

impl Equeue {
    // convenience functions
    pub fn call<F: Post>(&self, cb: F) -> Result<Id, Error>{
        Ok(
            self.alloc_from(cb)?
                .post()
        )
    }

    pub fn call_in<F: Post>(&self, delay: itick, cb: F) -> Result<Id, Error> {
        Ok(
            self.alloc_from(cb)?
                .delay(delay)
                .post()
        )
    }

    pub fn call_every<F: Post>(&self, period: itick, cb: F) -> Result<Id, Error> {
        Ok(
            self.alloc_from(cb)?
                .delay(period)
                .period(period)
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
            p = p.wrapping_add(alignup(
                size_of::<Ebuf>() + e.size(),
                Ebuf::ALIGN
            ))
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
                free_bytes += size_of::<Ebuf>() + (Ebuf::ALIGN << npw2);

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



