
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

mod util;
use util::*;


/// Event queue errors
#[derive(Debug)]
pub enum Error {
    NoMem
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::NoMem => write!(f, "Out of memory"),
        }
    }
}


/// Slab-internal pointer, with internalized generation count
#[derive(Copy, Clone)]
#[repr(transparent)]
struct Eptr(usize);

impl fmt::Debug for Eptr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // these really need to be in hex to be readable
        write!(f, "Eptr(0x{:x})", self.0)
    }
}

impl Eptr {
    // maximum alignment of internal allocations, this must be larger than
    // Eptr's alignment, and pointer alignment is a common alignment
    //
    // TODO should these take advantage of the npw2(event size) to be compressed
    // even further?
    //
    const ALIGN: usize = {
        let mut align = align_of::<Ebuf>();
        if align_of::<Eptr>() > align { align = align_of::<Eptr>() }
        if align_of::<*const usize>() > align { align = align_of::<*const usize>() }
        align
    };

    const fn null() -> Eptr {
        Eptr(0)
    }
}

impl Equeue {
    // Eptr interactions
    fn eptr_from<'a>(
        &'a self,
        eptr: Eptr,
        gen: usize,
        e: Option<&'a Ebuf>
    ) -> Eptr {
        let off = match e {
            Some(e) => unsafe {
                (e as *const Ebuf as *const u8)
                    .offset_from(self.slab.as_ptr())
                    as usize
                    / Eptr::ALIGN
            },
            None => 0,
        };

        // make sure our gen/off aren't overflowing
        debug_assert!(gen <= (1 << self.gnpw2));
        debug_assert!(off <= (1 << self.enpw2));

        let mask = !((1 << self.gnpw2+self.enpw2) - 1);
        Eptr((eptr.0 & mask) | (gen << self.enpw2) | off)
    }

    fn eptr_mark(&self, eptr: Eptr) -> usize {
        eptr.0 >> self.gnpw2+self.enpw2
    }

    fn eptr_set_mark(&self, eptr: Eptr, mark: usize) -> Eptr {
        debug_assert!(mark <= (1 << self.gnpw2));
        let mask = (1 << self.gnpw2+self.enpw2) - 1;
        Eptr((mark << self.gnpw2+self.enpw2) | (eptr.0 & mask))
    }

    fn eptr_inc_mark(&self, eptr: Eptr) -> Eptr {
        Eptr(eptr.0.wrapping_add(1 << self.gnpw2+self.enpw2))
    }

    fn eptr_gen(&self, eptr: Eptr) -> usize {
        let gmask = (1 << self.gnpw2) - 1;
        (eptr.0 >> self.enpw2) & gmask
    }

    fn eptr_set_gen(&self, eptr: Eptr, gen: usize) -> Eptr {
        debug_assert!(gen <= (1 << self.gnpw2));
        let mask = !(((1 << self.gnpw2) - 1) << self.enpw2);
        Eptr((gen << self.enpw2) | (eptr.0 & mask))
    }

    fn eptr_inc_gen(&self, eptr: Eptr) -> Eptr {
        Eptr(
            eptr.0
                .rotate_left(8*size_of::<Eptr>() as u32 - (self.gnpw2+self.enpw2) as u32)
                .wrapping_add(1 << 8*size_of::<Eptr>() as u32 - self.gnpw2 as u32)
                .rotate_right(8*size_of::<Eptr>() as u32 - (self.gnpw2+self.enpw2) as u32)
        )
    }

    fn eptr_as_ptr(&self, eptr: Eptr) -> *const Ebuf {
        let mask = (1 << self.enpw2) - 1;
        if eptr.0 & mask != 0 {
            &self.slab[(eptr.0 & mask) * Eptr::ALIGN]
                as *const u8 as *const Ebuf
        } else {
            ptr::null()
        }
    }

    unsafe fn eptr_as_mut_ptr(&self, eptr: Eptr) -> *mut Ebuf {
        self.eptr_as_ptr(eptr) as *mut Ebuf
    }

    fn eptr_as_ref<'a>(&'a self, eptr: Eptr) -> Option<&'a Ebuf> {
        unsafe { self.eptr_as_ptr(eptr).as_ref() }
    }

    unsafe fn eptr_as_mut<'a>(&'a self, eptr: Eptr) -> Option<&'a mut Ebuf> {
        self.eptr_as_mut_ptr(eptr).as_mut()
    }

    // Cas<Eptr> interactions
    fn eptr_load<'a>(&'a self, eptr: &Cas<Eptr>) -> Option<&'a Ebuf> {
        self.eptr_as_ref(eptr.load())
    }

    fn eptr_load_ex<'a>(
        &'a self,
        eptr: &mut Cas<Eptr>
    ) -> Option<&'a mut Ebuf> {
        unsafe { self.eptr_as_mut(eptr.load_ex()) }
    }

    // Sequential generation comparisons
    fn gen_sdiff(&self, a: usize, b: usize) -> isize {
        sdiff(a << self.gnpw2+self.enpw2, b << self.gnpw2+self.enpw2)
            >> self.gnpw2+self.enpw2
    }

    fn gen_scmp(&self, a: usize, b: usize) -> Ordering {
        scmp(a << self.gnpw2+self.enpw2, b << self.gnpw2+self.enpw2)
    }
}

/// Internal event header
#[derive(Debug)]
struct Ebuf {
    status: Cas<usize>,
    next: Cas<Eptr>,
    sibling: Cas<Eptr>,

    cb: Option<fn(*mut u8)>,
    drop: Option<fn(*mut u8)>,
    target: utick,
    period: itick,
}

impl Ebuf {
    const ACTIVE: usize = 0x100;

    // access to the trailing buffer
    unsafe fn as_ptr<T>(&self) -> *const T {
        (self as *const Ebuf).add(1) as *const T
    }

    unsafe fn as_mut_ptr<T>(&mut self) -> *mut T {
        (self as *mut Ebuf).add(1) as *mut T
    }

    fn as_ref<'a, T>(&'a self) -> &'a T {
        unsafe { &*self.as_ptr() }
    }

    fn as_mut<'a, T>(&'a mut self) -> &'a mut T {
        unsafe { &mut *self.as_mut_ptr() }
    }

    unsafe fn from_mut_ptr<'a, T>(ptr: *mut T) -> Option<&'a mut Ebuf> {
        if !ptr.is_null() {
            Some(&mut *(ptr as *mut Ebuf).sub(1))
        } else {
            None
        }
    }
}

impl Equeue {
    // we can "claim" an Ebuf once we remove it from shared structures,
    // the claim is unsafe, but afterwards we can leverage Rust's type
    // system to know whether or not we have exclusive access to the Ebuf
    unsafe fn ebuf_claim<'a>(&'a self, e: &'a Ebuf) -> &'a mut Ebuf {
        &mut *(e as *const Ebuf as *mut Ebuf)
    }

    // access the generation count embedded in our event's mark
//    fn ebuf_gen_load<'a>(&'a self, e: &'a Ebuf) -> usize {
//        self.eptr_mark(e.next.load())
////        // next can change before sibling, so sibling contains the "true"
////        // generation count
////        self.eptr_mark(e.sibling.load())
//    }
//
//    fn ebuf_gen_load_ex<'a>(&'a self, e: &'a mut Ebuf) -> usize {
//        self.eptr_mark(e.next.load_ex())
////        // next can change before sibling, so sibling contains the "true"
////        // generation count
////        self.eptr_mark(e.sibling.load_ex())
//    }

    fn ebuf_npw2(&self, e: &Ebuf) -> u8 {
        // npw2 doesn't actually change
        (unsafe { self.ebuf_claim(e) }.status.load_ex() & 0xff) as u8
    }

    fn ebuf_gen_inc_ex<'a>(&'a self, e: &'a mut Ebuf) {
        let status = e.status.load_ex().wrapping_add(1 << self.gnpw2+self.enpw2);
        e.status.store_ex(status);
        let nextptr = self.eptr_inc_mark(e.next.load_ex());
        e.next.store_ex(nextptr);
        // siblingptr may lag behind due to cancels
        let siblingptr = e.sibling.load_ex();
        debug_assert!(self.gen_scmp(
            self.eptr_mark(siblingptr),
            self.eptr_mark(nextptr)
        ).is_lt());
        self.eptr_set_mark(siblingptr, self.eptr_mark(nextptr));
        e.sibling.store_ex(siblingptr);
    }
}

/// Event queue struct
#[derive(Debug)]
pub struct Equeue {
    // memory management
    slab: &'static [u8],
    slab_front: Cas<usize>,
    slab_back: Cas<usize>,
    enpw2: u8,
    gnpw2: u8,

    // queue management
    queue: Cas<Eptr>,

    // other things
    clock: Clock,
    sema: Sema,
}

unsafe impl Send for Equeue {}
unsafe impl Sync for Equeue {}

impl Equeue {
    pub fn with_buffer(buffer: &'static mut [u8]) -> Result<Equeue, Error> {
        // align buffer
        let align = alignup(buffer.as_ptr() as usize, Eptr::ALIGN)
            - buffer.as_ptr() as usize;
        let buffer = match buffer.get_mut(align..) {
            // already overflow?
            Some(buffer) => buffer,
            None => return Err(Error::NoMem),
        };

        // go ahead and zero our buffer, this makes it easier to manage bucket
        // allocation, which needs to be null the moment a bucket is allocated
        buffer.fill(0);

        // find the maximum mark/gen field size in our eptrs, these are crammed
        // next to the offset, which we make as small as possible by leveraging
        // alignment and limiting references to our memory region
        let enpw2 = npw2(buffer.len()) - npw2(Eptr::ALIGN);
        let gnpw2 = (8*size_of::<Eptr>() as u8 - enpw2) / 2;

        // make sure there is some minimum generation width, but it's hard to
        // know what this needs to be...
        assert!(gnpw2 >= 4);

        Ok(Equeue {
            slab: buffer,
            slab_front: Cas::new(0),
            slab_back: Cas::new(buffer.len()),
            enpw2: enpw2,
            gnpw2: gnpw2,

            queue: Cas::new(Eptr::null()),

            clock: Clock::new(),
            sema: Sema::new(),
        })
    }

    pub fn now(&self) -> utick {
        self.clock.now()
    }

    fn buckets<'a>(&'a self) -> &'a [Cas<Eptr>] {
        let slab_front = self.slab_front.load();
        unsafe {
            slice::from_raw_parts(
                self.slab.as_ptr() as *const Cas<Eptr>,
                slab_front / size_of::<Eptr>()
            )
        }
    }

    fn contains_ebuf(&self, e: &Ebuf) -> bool {
        self.slab.as_ptr_range()
            .contains(&(e as *const _ as *const u8))
    }

    // Memory management
    fn alloc_ebuf<'a>(&'a self, layout: Layout) -> Result<&'a mut Ebuf, Error> {
        assert!(layout.align() <= Eptr::ALIGN);

        // find best bucket
        let npw2 = npw2((layout.size()+Eptr::ALIGN-1) / Eptr::ALIGN);

        'retry: loop {
            // 1. do we have an allocation in our buckets? we don't look
            // at larger buckets because those are likely to be reused, we don't
            // want to starve larger events with smaller events
            if let Some(bucket) = self.buckets().get(npw2 as usize) {
                // CAS try to take an event from a bucket
                let mut eptr = bucket.load();
                while let Some(e) = self.eptr_as_ref(eptr) {
                    let siblingptr = e.sibling.load();
                    if let Err(x) = bucket.cas(eptr, siblingptr) {
                        eptr = x;
                        continue;
                    }

                    let e = unsafe { self.ebuf_claim(e) };

                    // zero certain fields
                    e.cb = None;
                    e.drop = None;
                    e.target = 0;
                    e.period = -1;
                    return Ok(e);
                }
            }

            // 2. a litmus test, do we even have enough memory to satisfy
            // our request?
            let slab_front = self.slab_front.load();
            let slab_back = self.slab_back.load();
            let new_slab_front = max(
                (npw2 as usize + 1)*size_of::<Eptr>(),
                slab_front
            );
            let new_slab_back = aligndown(
                slab_back.saturating_sub(size_of::<Ebuf>() + (Eptr::ALIGN << npw2)),
                Eptr::ALIGN
            );

            if new_slab_front > new_slab_back {
                return Err(Error::NoMem);
            }

            // 3. make sure we have enough buckets allocated
            if new_slab_front > slab_front {
                // CAS to allocate buckets
                if let Err(_) = self.slab_front.cas(slab_front, new_slab_front) {
                    continue 'retry;
                }
            }

            // 4. allocate our new event
            //
            // note that if we fail here the only modification we've made is to our
            // bucket list, we may end up with unused buckets, but that's not the
            // end of the world. It's a sparse array anyways.
            //
            // CAS to allocate event
            debug_assert!(new_slab_back < slab_back);
            if let Err(_) = self.slab_back.cas(slab_back, new_slab_back) {
                continue 'retry;
            }

            unsafe {
                let e = &self.slab[new_slab_back] as *const u8 as *const Ebuf as *mut Ebuf;
                e.write(Ebuf {
                    status: Cas::new(0 | npw2 as usize),
                    next: Cas::new(Eptr::null()),
                    sibling: Cas::new(Eptr::null()),

                    cb: None,
                    drop: None,
                    target: 0,
                    period: -1,
                });

                return Ok(&mut *e);
            }
        }
    }

    fn dealloc_ebuf(&self, e: &mut Ebuf) {
        debug_assert!(self.contains_ebuf(e));
        // generation count are incremented during removal from the queue, but
        // they should be consistent, otherwise our algorithm has a bug
        debug_assert_eq!(
            self.eptr_mark(e.next.load_ex()),
            self.eptr_mark(e.sibling.load_ex())
        );

        // we only ever need to load buckets once because it can never shrink
        let bucket = &self.buckets()[self.ebuf_npw2(e) as usize];

        // CAS try to add our event to a bucket
        let gen = self.eptr_mark(e.next.load_ex());
        let mut siblingptr = bucket.load();
        loop {
            debug_assert_ne!(e as *const _, self.eptr_as_ptr(siblingptr));
            e.sibling.store_ex(self.eptr_set_mark(siblingptr, gen));

            let eptr = self.eptr_from(Eptr::null(), gen, Some(e));
            if let Err(x) = bucket.cas(siblingptr, eptr) {
                siblingptr = x;
                continue;
            }

            return;
        }
    }

    // Queue management
    fn enqueue_ebuf(&self, e: &mut Ebuf, target: utick) -> Eptr {
        debug_assert!(e.cb.is_some());
        // mark the event as executing
        // TODO here?
        let status = e.status.load_ex() | Ebuf::ACTIVE;
        e.status.store_ex(status);
        e.target = target;

        let gen = self.eptr_mark(e.next.load_ex());
        'retry: loop {
            let mut headptr = self.queue.load();
            let mut headsrc = &self.queue;

            // find insertion point
            let mut delta = Ordering::Greater;
            let mut nextptr = Eptr::null();
            let mut nextsrc = None;
            while let Some(head) = self.eptr_as_ref(headptr) {
                // generation mismatch? someone is trying to remove and we
                // need to help out
                nextptr = head.next.load();
                nextsrc = Some(&head.next);
                if self.eptr_mark(nextptr) != self.eptr_gen(headptr) {
//                    // make sure sibling is in sync with next
//                    let siblingptr = head.sibling.load();
//                    if self.eptr_mark(siblingptr) != self.eptr_mark(nextptr) {
//                        let dirty_siblingptr = self.eptr_set_mark(siblingptr, self.eptr_mark(nextptr));
//                        if let Err(_) = head.sibling.cas(siblingptr, dirty_siblingptr) {
//                            continue 'retry;
//                        }
//                    }

                    let nextptr = self.eptr_set_mark(nextptr, self.eptr_mark(headptr));
                    if let Err(_) = headsrc.cas(headptr, nextptr) {
                        // someone else removed the event for us, but we don't know
                        // what else has happened, so we need to retry
                        continue 'retry;
                    }

                    // if no one fixed this before us, we can continue
                    headptr = nextptr;
                    continue;
                }

                let ndelta = scmp(head.target, target);
                if ndelta.is_ge() {
                    delta = ndelta;
                    break;
                }

                headptr = nextptr;
                headsrc = &head.next;
            };

            // set ourselves up for insertion
            let siblingptr;
            let eptr;
            match delta {
                Ordering::Greater => {
                    // inserting a new slice, nothing complicated here
                    eptr = self.eptr_from(headptr, gen, Some(e));
                    nextptr = self.eptr_set_mark(headptr, gen);
                    siblingptr = self.eptr_set_mark(Eptr::null(), gen);
                }
                Ordering::Equal => {
                    // inserting onto an existing slice, which is a bit complicated
                    //
                    // We can't really replace the top event atomically, so instead
                    // we insert our event _after_ the existing slice, but with the
                    // existing slice in our sibling list, while also marking the
                    // existing slice to be removed. This creates duplicate
                    // entries, but as long as we fix removals before dispatching
                    // events things work out.
                    let predptr = headptr;
                    headptr = nextptr;
                    headsrc = nextsrc.unwrap();

                    eptr = self.eptr_from(self.eptr_inc_mark(headptr), gen, Some(e));
                    nextptr = self.eptr_set_mark(headptr, gen);
                    siblingptr = self.eptr_set_mark(predptr, gen);
                    //siblingptr = self.eptr_set_mark(self.eptr_inc_gen(predptr), gen);
                }
                Ordering::Less => unreachable!(),
            };
            debug_assert_ne!(e as *const _, self.eptr_as_ptr(nextptr));
            debug_assert_ne!(e as *const _, self.eptr_as_ptr(siblingptr));
            e.next.store_ex(nextptr);
            e.sibling.store_ex(siblingptr);

            // CAS try to insert our event into the queue
            if let Err(_) = headsrc.cas(headptr, eptr) {
                // if we fail here any number of things could have happened,
                // we need to completely restart
                continue 'retry;
            }

            // In _theory_, if we were pushing onto an existing slice, we should
            // go back and clean up the duplicate entries we created.
            //
            // But this gets complicated, and in practice, since any access to
            // the queue must fix pending removes, the duplicate entries will
            // get fixed before they have any effect on either behavior or
            // runtime

            // TODO this is a bit of a mess
            break self.eptr_set_gen(eptr, self.eptr_mark(Eptr(status)));
        }
    }

    fn unqueue_ebuf<'a>(&'a self, eptr: Eptr) -> bool { //Option<&'a mut Ebuf> {
        let gen = self.eptr_gen(eptr);
        let e = match self.eptr_as_ref(eptr)
            // I supposed eptrs could be passed between equeues, it's probably
            // an error to do so, but the correct action is to ignore the request
            .filter(|e| self.contains_ebuf(e))
        {
            Some(e) => e,
            None => return false,
        };

        // first, update the status to indicate we're canceled
        let mut status = e.status.load();
        let canceled = loop {
            if self.eptr_mark(Eptr(status)) != gen {
                println!("was {:#x} is {:#x} ({:#x})", gen, self.eptr_mark(Eptr(status)), status);
                break false;
                //break true;  // TODO what the heck
            }

            let dirty_status = status & !Ebuf::ACTIVE;
            if let Err(x) = e.status.cas(status, dirty_status) {
                status = x;
                continue;
            }

            break true;
        };

        // now try to find our event and reclaim the memory
        let target = e.target;
        let mut found = false;
        let mut defered_clean = None;
        'retry: loop {
            let mut headptr = self.queue.load();
            let mut headsrc = &self.queue;

            // find insertion point
            let mut delta = Ordering::Greater;
            let mut headnextptr = None;
            while let Some(head) = self.eptr_as_ref(headptr) {
                // generation mismatch? someone is trying to remove and we
                // need to help out
                let nextptr = head.next.load();
                headnextptr = Some(nextptr);
                if self.eptr_mark(nextptr) != self.eptr_gen(headptr) {
//                    // make sure sibling is in sync with next
//                    let siblingptr = head.sibling.load();
//                    if self.eptr_mark(siblingptr) != self.eptr_mark(nextptr) {
//                        let dirty_siblingptr = self.eptr_set_mark(siblingptr, self.eptr_mark(nextptr));
//                        if let Err(_) = head.sibling.cas(siblingptr, dirty_siblingptr) {
//                            continue 'retry;
//                        }
//                    }

                    let nextptr = self.eptr_set_mark(nextptr, self.eptr_mark(headptr));
                    if let Err(_) = headsrc.cas(headptr, nextptr) {
                        // someone else removed the event for us, but we don't know
                        // what else has happened, so we need to retry
                        continue 'retry;
                    }

                    // if no one fixed this before us, we can continue
                    headptr = nextptr;
                    continue;
                }

                let ndelta = scmp(head.target, target);
                if ndelta.is_ge() {
                    delta = ndelta;
                    break;
                }

                headptr = nextptr;
                headsrc = &head.next;
            };

            // TODO wow this needs to be restructured
            if let Some(cleanme) = defered_clean {
                unreachable!();
                let e = unsafe { self.ebuf_claim(cleanme) };
    //            // depending on which path we took for removal, sibling may be out of date
    //            let nextptr = e.next.load_ex();
    //            let siblingptr = e.sibling.load_ex();
    //            if self.gen_scmp(self.eptr_mark(siblingptr), self.eptr_mark(nextptr)).is_lt() {
    //                e.sibling.store_ex(self.eptr_set_mark(siblingptr, self.eptr_mark(nextptr)));
    //            }

                // TODO should this be common?
//                let status = e.status.load_ex().wrapping_add(1 << self.gnpw2+self.enpw2);
//                e.status.store_ex(status);
                self.ebuf_gen_inc_ex(e);
                if let Some(drop) = e.drop {
                    drop(unsafe { e.as_mut_ptr() });
                }
                self.dealloc_ebuf(e);

                defered_clean = None
            }

            // found the timeslice, try to find the actual event
            let mut first = true;
            if delta.is_eq() {
                while let Some(head) = self.eptr_as_ref(headptr) {
                    // generation mismatch?
                    let siblingptr = head.sibling.load();
                    match (
                        self.gen_scmp(self.eptr_mark(siblingptr), self.eptr_gen(headptr)),
                        first,
                        self.eptr_as_ref(siblingptr)
                    ) {
                        (Ordering::Equal, _, _) => {}
                        (Ordering::Greater, true, Some(sibling)) => {
//                            break 'retry;
//                            unreachable!();

                            // head of slice? this gets a bit tricky, but we can
                            // move our sibling up next to us, modifying the sibling's
                            // next pointer which shouldn't be used, and atomically
                            // marking ourself for removal
                            let nextptr = headnextptr.unwrap();

        // TODO how do we resolve this
        //
        // 1. if we insert before we can remove this, we end up with a
        // sibling node marked for removal, that's not good!
        //
        // 2. say we either increment siblingptr when we mark this, or
        // do the whole atomic swap thing, we end up with a siblingptr that,
        // if incremented, is not actually available to be freed... that's
        // a problem!
        //
        // if we were trying to free the sibling's sibling, we would think
        // we marked it to be free but we wouldn't actually!
         
//                            // setup siblingsiblingptr
//                            let siblingsiblingptr = sibling.sibling.load();
//                            if self.eptr_mark(siblingsiblingptr)
//                                == self.eptr_gen(siblingptr)
//                            {
//                                let nsiblingsiblingptr = self.eptr_inc_mark(siblingsiblingptr);
//                                if let Err(_) = sibling.sibling.cas(siblingsiblingptr, nsiblingsiblingptr) {
//                                    continue 'retry;
//                                }
//                            } else if self.eptr_mark(siblingsiblingptr)
//                                    != self.eptr_gen(self.eptr_inc_gen(siblingptr)) {
//                                // we may have already been dispatched
//                                continue 'retry;
//                            }

                            // setup siblingnextptr
                            let siblingnextptr = sibling.next.load();
                            if self.eptr_mark(siblingnextptr)
                                != self.eptr_gen(self.eptr_inc_gen(siblingptr))
                            {
                                //println!("hmm");
                                // we may have already been dispatched
                                continue 'retry;
                            }
//                            let nsiblingnextptr = self.eptr_inc_mark(self.eptr_set_mark(
//                                nextptr,
//                                self.eptr_mark(siblingnextptr),
//                            ));
                            let nsiblingnextptr = self.eptr_set_mark(
                                nextptr,
                                self.eptr_mark(siblingnextptr),
                            );
                            if let Err(_) = sibling.next.cas(siblingnextptr, nsiblingnextptr) {
                                continue 'retry;
                            }

                            // try to actually remove the event
                            let dirty_nextptr = self.eptr_from(
                                self.eptr_inc_mark(nextptr),
                                self.eptr_mark(nsiblingnextptr),
                                Some(sibling)
                            );
                            if let Err(_) = head.next.cas(nextptr, dirty_nextptr) {
                                continue 'retry;
                            }

// TODO
//                            // managed to claim the removal!
//                            defered_clean = Some(head);
//
                            // TODO are large backwards jumps the only way to do this?
                            // we don't necessarily need to restart completely...
                            continue 'retry;

//                            break 'retry;
                        }
                        (Ordering::Greater, true, None) => { 
                            //unreachable!();

                            // head of slice but no sibling? we can just mark
                            // ourself for removal then
                            //
                            // remember that adding new siblings works by
                            // updating our next pointer first, which we would
                            // clobber
                            let nextptr = headnextptr.unwrap();
                            let dirty_nextptr = self.eptr_inc_mark(nextptr);
                            if let Err(_) = head.next.cas(nextptr, dirty_nextptr) {
                                continue 'retry;
                            }

// TODO
//                            // managed to claim the removal!
//                            defered_clean = Some(head);

                            // TODO are large backwards jumps the only way to do this?
                            // we don't necessarily need to restart completely...
                            continue 'retry;
                        }
                        (Ordering::Greater, false, _) => {
                            //unreachable!();

        //                    // make sure sibling is in sync with sibling
        //                    let siblingptr = head.sibling.load();
        //                    if self.eptr_mark(siblingptr) != self.eptr_mark(siblingptr) {
        //                        let dirty_siblingptr = self.eptr_set_mark(siblingptr, self.eptr_mark(siblingptr));
        //                        if let Err(_) = head.sibling.cas(siblingptr, dirty_siblingptr) {
        //                            continue 'retry;
        //                        }
        //                    }

                            // someone was trying to remove this sibling, we
                            // need to help out
                            let siblingptr = self.eptr_set_mark(siblingptr, self.eptr_mark(headptr));
                            if let Err(_) = headsrc.cas(headptr, siblingptr) {
                                // someone else removed the event for us, but we don't know
                                // what else has happened, so we need to retry
                                continue 'retry;
                            }

                            // managed to remove the event
                            //
                            // ownership of siblings is a bit different than next pointers,
                            // we now own the memory and need to clean it up
//                            {
//                                let e = unsafe { self.ebuf_claim(head) };
//                    //            // depending on which path we took for removal, sibling may be out of date
//                    //            let nextptr = e.next.load_ex();
//                    //            let siblingptr = e.sibling.load_ex();
//                    //            if self.gen_scmp(self.eptr_mark(siblingptr), self.eptr_mark(nextptr)).is_lt() {
//                    //                e.sibling.store_ex(self.eptr_set_mark(siblingptr, self.eptr_mark(nextptr)));
//                    //            }
//
//                                // TODO should this be common?
////                                let status = e.status.load_ex().wrapping_add(1 << self.gnpw2+self.enpw2);
////                                e.status.store_ex(status);
//                                self.ebuf_gen_inc_ex(e);
//                                if let Some(drop) = e.drop {
//                                    drop(unsafe { e.as_mut_ptr() });
//                                }
//                                self.dealloc_ebuf(e);
//                            }

                            // if no one fixed this before us, we can continue
                            headptr = siblingptr;
                            continue;
                        }
                        (Ordering::Less, _, _) => {
                            //unreachable!()

                            // transient state when disentangling during cancel,
                            // fix and continue
                            let fixed_siblingptr = self.eptr_set_mark(siblingptr, self.eptr_gen(headptr));
                            if let Err(_) = head.sibling.cas(siblingptr, fixed_siblingptr) {
                                continue;
                            }
                        }
                    }

                    // TODO check for match?
//                    let ndelta = scmp(head.target, target).cmp(&0);
//                    if ndelta.is_ge() {
//                        delta = ndelta;
//                        break;
//                    }

                    // found our event? try to remove it
                    // TODO what if we mark it here, but it's been freed, and
                    // recycled since we checked the generation count??? do we need
                    // to load the sibling mark before canceling this event? or
                    // remove from queue first?
                    if head as *const _ == e as *const _
                        && self.eptr_mark(Eptr(e.status.load())) == gen
                    {
                        match (first, self.eptr_as_ref(siblingptr)) {
                            (true, Some(sibling)) => {
                                // head of slice? this gets a bit tricky, but we can
                                // move our sibling up next to us, modifying the sibling's
                                // next pointer which shouldn't be used, and atomically
                                // marking ourself for removal
//                                let nextptr = headnextptr.unwrap();
//                                let siblingnextptr = sibling.next.load();
//                                debug_assert_eq!(
//                                    self.eptr_mark(siblingnextptr),
//                                    self.eptr_gen(self.eptr_inc_gen(siblingptr)),
//                                );
//                                let nsiblingnextptr = self.eptr_set_mark(
//                                    nextptr,
//                                    self.eptr_mark(siblingnextptr),
//                                );
//                                if let Err(_) = sibling.next.cas(siblingnextptr, nsiblingnextptr) {
//                                    continue 'retry;
//                                }

//                                let dirty_nextptr = self.eptr_from(
//                                    self.eptr_inc_mark(nextptr),
//                                    self.eptr_mark(nsiblingnextptr),
//                                    Some(sibling)
//                                );
//                                if let Err(_) = head.next.cas(nextptr, dirty_nextptr) {
//                                    continue 'retry;
//                                }
//
//                                // managed to claim the removal!
//                                found = true;
//                                // TODO if we move the found in head position
//                                // up, can we avoid these big backwards jumps?
//                                continue 'retry;

                                // head of slice? mark it for removal and let the
                                // loop clean it up
                                let dirty_siblingptr = self.eptr_inc_mark(self.eptr_set_mark(siblingptr, self.eptr_gen(headptr)));
                                if let Err(_) = head.sibling.cas(siblingptr, dirty_siblingptr) {
                                    continue 'retry;
                                }

                                // managed to claim the removal!
                                found = true;
                                continue;
                            }
                            (true, None) => {
//                                // head of slice but no sibling? we can just mark
//                                // ourself for removal then
//                                //
//                                // remember that adding new siblings works by
//                                // updating our next pointer first, which we would
//                                // clobber
//                                let nextptr = headnextptr.unwrap();
//                                let dirty_nextptr = self.eptr_inc_mark(nextptr);
//                                if let Err(_) = head.next.cas(nextptr, dirty_nextptr) {
//                                    continue 'retry;
//                                }

                                // head of slice but no siblings? mark it for
                                // removal and let the loop clean it up
                                let dirty_siblingptr = self.eptr_inc_mark(self.eptr_set_mark(siblingptr, self.eptr_gen(headptr)));
                                if let Err(_) = head.sibling.cas(siblingptr, dirty_siblingptr) {
                                    continue 'retry;
                                }

                                // managed to claim the removal!
                                found = true;
                                continue;
                            }
                            (false, _) => {
                                // not head of slice? just mark it for removal and
                                // let the loop clean it up
                                let dirty_siblingptr = self.eptr_inc_mark(self.eptr_set_mark(siblingptr, self.eptr_gen(headptr)));
                                if let Err(_) = head.sibling.cas(siblingptr, dirty_siblingptr) {
                                    continue 'retry;
                                }

                                // managed to claim the removal!
                                found = true;
                                continue;
                            }
                        }
                    }

                    headptr = siblingptr;
                    headsrc = &head.sibling;
                    first = false;
                };
            }

            break;
        }

        // make sure we clean up the memory
//        if found {
//            let e = unsafe { self.ebuf_claim(e) };
////            // depending on which path we took for removal, sibling may be out of date
////            let nextptr = e.next.load_ex();
////            let siblingptr = e.sibling.load_ex();
////            if self.gen_scmp(self.eptr_mark(siblingptr), self.eptr_mark(nextptr)).is_lt() {
////                e.sibling.store_ex(self.eptr_set_mark(siblingptr, self.eptr_mark(nextptr)));
////            }
//
//            // TODO should this be common?
//            let status = e.status.load_ex().wrapping_add(1 << self.gnpw2+self.enpw2);
//            e.status.store_ex(status);
//            if let Some(drop) = e.drop {
//                drop(unsafe { e.as_mut_ptr() });
//            }
//            self.dealloc_ebuf(e);
//        }

        canceled





//        todo!()
//
//        // 1. is our event even in-flight anymore?
//        let mut siblingptr = e.sibling.load();
//        loop {
//            if self.eptr_mark(siblingptr) != gen {
//                return None;
//            }
//
//            // 2. atomically mark ourselves for removal, if we succeed here,
//            // we now own the memory
//            let dirty_siblingptr = self.eptr_inc_mark(siblingptr);
//            if let Err(x) = e.sibling.cas(siblingptr, dirty_siblingptr) {
//                siblingptr = x;
//                continue;
//            }
//
//            break;
//        }
//
//        // 3. now for the expensive part, before we can actually reuse the
//        // memory, we need to make sure the event is no longer in the queue
//        //
//        // we actually don't need to find our specific event, if we go through
//        // the queue and clean up any in-flight removals for our time slice,
//        // then we can be sure our event has been removed
//        let target = e.target;
//        'retry: loop {
//            let mut headptr = self.queue.load();
//            let mut headsrc = &self.queue;
//
//            // find time-slice
//            let mut delta = Ordering::Greater;
//            while let Some(head) = self.eptr_as_ref(headptr) {
//                // generation mismatch? someone is trying to remove and we
//                // need to help out
//                let nextptr = head.next.load();
//                if self.eptr_mark(nextptr) != self.eptr_gen(headptr) {
//                    let nextptr = self.eptr_set_mark(nextptr, self.eptr_mark(headptr));
//                    if let Err(_) = headsrc.cas(headptr, nextptr) {
//                        // someone else removed the event for us, but we don't know
//                        // what else has happened, so we need to retry
//                        continue 'retry;
//                    }
//
//                    // if no one fixed this before us, we can continue
//                    headptr = nextptr;
//                    continue;
//                }
//
//                let ndelta = scmp(head.target, target).cmp(&0);
//                if ndelta.is_ge() {
//                    delta = ndelta;
//                    break;
//                }
//
//                headptr = nextptr;
//                headsrc = &head.next;
//            };
//
//            if delta.is_eq() {
//                // found our time-slice, let's find our event
//                while let Some(head) = self.eptr_as_ref(headptr) {
//                    // generation mismatch? someone is trying to remove and we
//                    // need to help out
//                    let mut siblingptr = head.sibling.load();
//                    if self.eptr_mark(siblingptr) != self.eptr_gen(headptr) {
//                        // first we need to make sure our event isn't in the next list, or
//                        // it has no siblings, we can do this by replacing our next pointer
//                        // with our sibling pointer while marking ourselves for deletion, a
//                        // sort of reverse slice squish
//                        //
//                        // note if we're already removed from the next list, either through
//                        // another cancel or slice squish, our mark/gen should mismatch
//                        let nextptr = head.next.load();
//                        if self.eptr_mark(nextptr) == self.eptr_gen(headptr) {
//                            match self.eptr_as_ref(siblingptr) {
//                                Some(sibling) => {
//                                    let nextptr = self.eptr_set_mark(nextptr, self.eptr_gen(siblingptr));
//                                    if let Err(_) = sibling.next.cas(sibling.next.load(), nextptr) {
//                                        // yes this can fail! thanks to cancel, very complicated
//                                        // interactions here are possible
//                                        continue 'retry;
//                                    }
//                                }
//                                None => {
//                                    siblingptr = self.eptr_inc_mark(nextptr);
//                                }
//                            };
//                            if let Err(_) = e.next.cas(nextptr, siblingptr) {
//                                continue 'retry;
//                            }
//
//                            // TODO hm
//                            continue 'retry;
//                        }
//
//                        // now we can remove ourself from the siblings/next (headsrc can be either!)
//                        let siblingptr = self.eptr_set_mark(siblingptr, self.eptr_mark(headptr));
//                        if let Err(_) = headsrc.cas(headptr, siblingptr) {
//                            // someone else removed the event for us, but we don't know
//                            // what else has happened, so we need to retry
//                            continue 'retry;
//                        }
//
//                        // if no one fixed this before us, we can continue
//                        headptr = siblingptr;
//                        continue;
//                    }
//
//                    // found our event? we're done
//                    if head as *const _ == e as *const _ {
//                        break;
//                    }
//
//                    headptr = siblingptr;
//                    headsrc = &head.sibling;
//                }
//            }
//
//            break;
//        }
//
//        // and now we are free to do whatever we want with this ebuf
//        Some(unsafe { self.ebuf_claim(e) })
////
////        // let's just make our generation counts consistent first
////        let nextptr = self.eptr_set_mark(e.next.load_ex(), self.eptr_mark(siblingptr));
////        e.next.store_ex(nextptr);
////
////        Some(e)
    }

    fn dequeue_ebufs<'a>(&'a self, now: utick) -> Result<&'a mut Ebuf, itick> {
        let mut dequeuedptr = Cas::new(Eptr::null());
        let mut dequeuedtail = &dequeuedptr;
        let mut dequeuedtailptr = Eptr::null();
        let delta = 'retry: loop {
            let mut headptr = self.queue.load();
            while let Some(head) = self.eptr_as_ref(headptr) {
                // generation mismatch? someone is trying to remove and we
                // need to help out
                let nextptr = head.next.load();
                if self.eptr_mark(nextptr) != self.eptr_gen(headptr) {
//                    // make sure sibling is in sync with next
//                    let siblingptr = head.sibling.load();
//                    if self.eptr_mark(siblingptr) != self.eptr_mark(nextptr) {
//                        let dirty_siblingptr = self.eptr_set_mark(siblingptr, self.eptr_mark(nextptr));
//                        if let Err(_) = head.sibling.cas(siblingptr, dirty_siblingptr) {
//                            continue 'retry;
//                        }
//                    }

                    let nextptr = self.eptr_set_mark(nextptr, self.eptr_mark(headptr));
                    if let Err(_) = self.queue.cas(headptr, nextptr) {
                        // someone else removed the event for us, but we don't know
                        // what else has happened, so we need to retry
                        continue 'retry;
                    }

                    // if no one fixed this before us, we can continue
                    headptr = nextptr;
                    continue;
                }

                // is this slice ready to dispatch?
                let delta = sdiff(head.target, now);
                if delta > 0 {
                    // no? return how long until the next event
                    break 'retry delta;
                }

                // ok, the slice is ready to dispatch
                //
                // CAS 1. mark the slice for removal
                let dirty_nextptr = self.eptr_inc_mark(nextptr);
                if let Err(_) = head.next.cas(nextptr, dirty_nextptr) {
                    continue 'retry;
                }
//                let nextptr = dirty_nextptr;

//                // CAS 2. keep the next/sibling pointers in sync
//                let mut siblingptr = head.sibling.load();
//                loop {
//                    if self.eptr_mark(siblingptr) != self.eptr_mark(nextptr) {
//                        let dirty_siblingptr = self.eptr_set_mark(siblingptr, self.eptr_mark(nextptr));
//                        if let Err(x) = head.sibling.cas(siblingptr, dirty_siblingptr) {
//                            siblingptr = x;
//                            continue;
//                        }
//                        siblingptr = dirty_siblingptr;
//                    }
//                    break;
//                }
//
//                // TODO is there a better way to structure this?
//
//                // since we've updated siblingptr, we need to keep headptr in sync
//                // TODO what's going on...
//                let headptr = self.eptr_set_gen(headptr, self.eptr_mark(siblingptr));

                // we've effectively removed the slice, but can't claim the
                // memory yet
                //
                // but, if we continue around this loop until all events sooner
                // then our target have been processed, we can be sure that the
                // event will actually be removed
                
                // to actually keep track of all these events, stick them on
                // an ad-hoc queue, as we build up the queue, we need to make
                // sure to mark+remove the sibling pointers since cancels can
                // still happen to events in here
                //
                // note that we're also inverting our slices, which act sort of
                // like stacks and are stored in reverse order
                let mut headptr = headptr;
                let mut head = head;
                let mut predptr = Eptr::null();
                let mut tail = None;
                let mut siblingptr = head.sibling.load();
                loop {
                    // TODO if we don't need this, can this whole loop be simplified?
//                    // generation mismatch?
//                    match self.gen_scmp(self.eptr_mark(siblingptr), self.eptr_gen(headptr)) {
//                        Ordering::Equal => {}
//                        Ordering::Greater => {
//                            // someone was trying to remove this sibling,
//                            // we can skip this
//                            match self.eptr_as_ref(siblingptr) {
//                                Some(sibling) => {
//                                    headptr = siblingptr;
//                                    head = sibling;
//                                }
//                                None => break,
//                            };
//                            siblingptr = head.sibling.load();
//                            continue;
//                        }
//                        Ordering::Less => {
//                            // transient state when disentangling during cancel,
//                            // continue, and we'll fix this when we replace the
//                            // sibling pointer
//                        }
//                    }

                    // TODO do we actually need double increments?

                    // CAS try to update with our predptr, effectively marking
                    // the event as removed
//                    let npredptr = self.eptr_inc_mark(self.eptr_inc_mark(
//                        self.eptr_set_mark(
//                            predptr,
//                            self.eptr_gen(headptr)
//                        )
//                    ));
                    let npredptr = self.eptr_inc_mark(
                        self.eptr_set_mark(
                            predptr,
                            self.eptr_gen(headptr)
                        )
                    );
                    //predptr = self.eptr_set_mark(predptr, self.eptr_mark(siblingptr));
                    if let Err(x) = head.sibling.cas(siblingptr, npredptr) {
                        siblingptr = x;
                        //println!("oh noooooooooooooooooooooooooooooooooooo");
                        continue;
                    }
//
//                    // TODO this needed?
//                    // CAS to keep next pointer in sync
//                    let nextptr = head.next.load();
//                    let dirty_nextptr = self.eptr_inc_mark(self.eptr_inc_mark(
//                        self.eptr_set_mark(
//                            nextptr,
//                            self.eptr_gen(headptr)
//                        )
//                    ));
//                    if let Err(_) = head.next.cas(nextptr, dirty_nextptr) {
//                        continue;
//                    }

//                    // TODO is this a hack
//                    // depending on which path we took for removal, sibling may be out of date
//                    let nextptr = head.next.load();
//                    if self.gen_scmp(self.eptr_mark(nextptr), self.eptr_mark(siblingptr)).is_lt() {
//                        head.next.cas(nextptr, self.eptr_set_mark(nextptr, self.eptr_mark(siblingptr))).unwrap();
//                    }

//                    // note that the generation count in the next pointer should
//                    // already be incremented due to either our removal above,
//                    // or insertion during slice squishing
//                    debug_assert_eq!(
//                        self.eptr_mark(predptr),
//                        self.eptr_mark(head.next.load())
//                    );


                    predptr = npredptr;

                    // head is valid, make sure to track it
                    tail = Some((
                        headptr,
                        tail.map(|(_, x, _)| x).unwrap_or(&head.sibling),
                        tail.map(|(_, _, x)| x).unwrap_or(predptr)
                    ));

                    match self.eptr_as_ref(siblingptr) {
                        Some(sibling) => {
//                            println!("up? {:?} {:?}", headptr, siblingptr);
                            predptr = headptr;
                            headptr = siblingptr;
                            head = sibling;
                        }
                        None => break,
                    }
                    siblingptr = head.sibling.load();
                }

                // stick on dequeued list
                if let Some((headptr, ndequeuedtail, ndequeuedtailptr)) = tail  {
                    let headptr = self.eptr_set_mark(
                        headptr,
                        self.eptr_mark(dequeuedtailptr)
                    );
                    dequeuedtail.cas(dequeuedtailptr, headptr).unwrap();
                    dequeuedtail = ndequeuedtail;
                    dequeuedtailptr = ndequeuedtailptr;
                }

                // continue to fix remove/find more slices
                continue 'retry;
            }

            // nothing to do I guess
            break 'retry -1;
        };

        // did we find any slices to dequeue?
        match self.eptr_as_ref(dequeuedptr.load_ex()) {
            Some(dequeued) => Ok(unsafe { self.ebuf_claim(dequeued) }),
            None => Err(delta),
        }
    }

    // Central post function
    fn post_ebuf(&self, e: &mut Ebuf, target: utick) -> Eptr {
        let eptr = self.enqueue_ebuf(e, target);

        // signal queue has changed
        self.sema.signal();

        eptr
    }

    // Central dispatch function
    pub fn dispatch(&self, ticks: itick) {
        // get the current time
        let mut now = self.clock.now();
        let timeout = now.wrapping_add(ticks as u64);

        loop {
            // get a slice to dispatch
            let mut slice = self.dequeue_ebufs(now).ok();

            let mut found = 0;
            let mut count = 0;

            while let Some(e) = slice {
                // first, increment the event's generation count,
                // this is the last chance for the even to be canceled
                let mut status = e.status.load();
                loop {
                    let dirty_status = status.wrapping_add(1 << self.gnpw2+self.enpw2);
                    if let Err(x) = e.status.cas(status, dirty_status) {
                        status = x;
                        continue;
                    }
                    break;
                }

                found += 1;

                if status & Ebuf::ACTIVE != 0 {
                    count += 1;

                    // dispatch!
                    e.cb.unwrap()(unsafe { e.as_mut_ptr() });
                }

                // move to next event
                slice = self.eptr_load_ex(&mut e.sibling);

                if status & Ebuf::ACTIVE != 0 && e.period >= 0 {
                    // reenqueue?
                    self.enqueue_ebuf(
                        e,
                        self.clock.now().wrapping_add(e.period as u64)
                    );
                } else {
                    // call drop, return event to memory pool
//                    if let Some(drop) = e.drop {
//                        drop(unsafe { e.as_mut_ptr() });
//                    }
//                    self.dealloc_ebuf(e);
                }
            }

            println!("found {}/{}", count, found);

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
            let mut delay = match self.eptr_as_ref(self.queue.load()) {
                Some(head) => max(sdiff(head.target, now), 0),
                None => -1,
            };

            if (delay as utick) > (timeout_left as utick) {
                delay = timeout_left;
            }

            self.sema.wait(delay);

            // update current time
            now = self.clock.now();
        }
    }

    // Central cancel function
    pub fn cancel(&self, id: Id) -> bool {
        self.unqueue_ebuf(Eptr::from(id))
//        match self.unqueue_ebuf(Eptr::from(id)) {
//            Some(e) => {
//                // make sure we clean up the event
//                if let Some(drop) = e.drop {
//                    drop(unsafe { e.as_mut_ptr() });
//                }
//                self.dealloc_ebuf(e);
//
//                true
//            }
//            None => {
//                false
//            }
//        }
    }

    // Handling of raw allocations
    pub unsafe fn alloc_raw(&self, layout: Layout) -> *mut u8 {
        match self.alloc_ebuf(layout) {
            Ok(e) => e.as_mut_ptr(),
            Err(_) => ptr::null_mut(),
        }
    }

    pub unsafe fn dealloc_raw(&self, e: *mut u8, _layout: Layout) {
        let e = match Ebuf::from_mut_ptr(e) {
            Some(e) => e,
            None => return, // do nothing
        };

        // make sure we call drop!
        if let Some(drop) = e.drop {
            drop(e.as_mut_ptr());
        }

        // if we don't post, we need to increment our generation here
        self.ebuf_gen_inc_ex(e);
        self.dealloc_ebuf(e);
    }

    pub fn contains_raw(&self, e: *mut u8) -> bool {
        match unsafe { Ebuf::from_mut_ptr(e) } {
            Some(e) => self.contains_ebuf(e),
            None => false,
        }
    }

    pub unsafe fn set_raw_drop(&self, e: *mut u8, drop: fn(*mut u8)) {
        let mut e = Ebuf::from_mut_ptr(e).unwrap();
        debug_assert!(self.contains_ebuf(e));
        e.drop = Some(drop);
    }

    pub unsafe fn set_raw_delay(&self, e: *mut u8, delay: itick) {
        let mut e = Ebuf::from_mut_ptr(e).unwrap();
        debug_assert!(self.contains_ebuf(e));
        debug_assert!(delay >= 0);
        e.target = delay as utick;
    }

    pub unsafe fn set_raw_period(&self, e: *mut u8, period: itick) {
        let mut e = Ebuf::from_mut_ptr(e).unwrap();
        debug_assert!(self.contains_ebuf(e));
        e.period = period;
    }

    pub unsafe fn post_raw(&self, cb: fn(*mut u8), e: *mut u8) -> Id {
        let mut e = Ebuf::from_mut_ptr(e).unwrap();
        debug_assert!(self.contains_ebuf(e));
        e.cb = Some(cb);
        let eptr = self.post_ebuf(e,
            self.clock.now().wrapping_add(e.target)
        );
        Id::try_from(eptr).unwrap()
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
pub struct Id(NonZeroUsize);

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // these really need to be in hex to be readable
        write!(f, "Id(0x{:x})", self.0)
    }
}

impl TryFrom<Eptr> for Id {
    type Error = TryFromIntError;
    fn try_from(eptr: Eptr) -> Result<Id, Self::Error> {
        NonZeroUsize::try_from(eptr.0).map(Id)
    }
}

impl From<Id> for Eptr {
    fn from(id: Id) -> Eptr {
        Eptr(usize::from(id.0))
    }
}


/// Event handle
#[derive(Debug)]
pub struct Event<'a, T> {
    q: &'a Equeue,
    e: &'a mut Ebuf,
    _phantom: PhantomData<T>,
}

impl<'a, T> Event<'a, T> {
    fn from_ebuf(q: &'a Equeue, e: &'a mut Ebuf) -> Event<'a, T> {
        debug_assert!(q.contains_ebuf(e));
        Event {
            q: q,
            e: e,
            _phantom: PhantomData,
        }
    }

    pub unsafe fn from_raw_parts(q: &'a Equeue, e: *mut T) -> Event<'a, T> {
        debug_assert!(!e.is_null());
        let e = Ebuf::from_mut_ptr(e).unwrap();
        Self::from_ebuf(q, e)
    }
}

impl Equeue {
    pub fn alloc<'a, T: Default>(&'a self) -> Result<Event<'a, T>, Error> {
        let e = self.alloc_ebuf(Layout::new::<T>())?;
        unsafe { e.as_mut_ptr::<T>().write(T::default()); }
        Ok(Event::from_ebuf(self, e))
    }

    pub fn alloc_from<'a, T>(&'a self, t: T) -> Result<Event<'a, T>, Error> {
        let e = self.alloc_ebuf(Layout::new::<T>())?;
        unsafe { e.as_mut_ptr::<T>().write(t); }
        Ok(Event::from_ebuf(self, e))
    }

    // TODO should we even have this? it shouldn't be possible to pass
    // events between unrelated equeues...
    pub fn post<T: Post>(&self, e: Event<'_, T>) -> Id {
        // cb/drop thunks
        fn cb_thunk<T: Post>(e: *mut u8) {
            unsafe { &mut *(e as *mut T) }.post();
        }

        fn drop_thunk<T>(e: *mut u8) {
            unsafe { drop_in_place(e as *mut T) };
        }

        e.e.cb = Some(cb_thunk::<T>);
        e.e.drop = Some(drop_thunk::<T>);

        // enqueue and then forget the event, it's up to equeue to
        // drop the event later
        let eptr = e.q.post_ebuf(e.e,
            self.clock.now().wrapping_add(e.e.target)
        );
        forget(e);

        Id::try_from(eptr).unwrap()
    }
}

impl<T: Post> Event<'_, T> {
    pub fn delay(self, delay: itick) -> Self {
        debug_assert!(delay >= 0);
        self.e.target = delay as utick;
        self
    }

    pub fn period(self, period: itick) -> Self {
        self.e.period = period;
        self
    }

    pub fn post(self) -> Id {
        self.q.post(self)
    }
}

impl<T> Drop for Event<'_, T> {
    fn drop(&mut self) {
        // make sure we clean up if the event isn't dispatched
        unsafe { drop_in_place(self.e.as_mut_ptr::<T>()) };

        // if we don't post, we need to increment our generation here
        self.q.ebuf_gen_inc_ex(self.e);
        self.q.dealloc_ebuf(self.e);
    }
}

impl<T> Deref for Event<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.e.as_ref()
    }
}

impl<T> DerefMut for Event<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.e.as_mut()
    }
}

impl<T> AsRef<T> for Event<'_, T> {
    fn as_ref(&self) -> &T {
        self.e.as_ref()
    }
}

impl<T> AsMut<T> for Event<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        self.e.as_mut()
    }
}

impl<T> Borrow<T> for Event<'_, T> {
    fn borrow(&self) -> &T {
        self.e.as_ref()
    }
}

impl<T> BorrowMut<T> for Event<'_, T> {
    fn borrow_mut(&mut self) -> &mut T {
        self.e.as_mut()
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


#[derive(Debug)]
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
        let slab_front = self.slab_front.load();
        let slab_back = self.slab_back.load();
        let slab_unused = slab_back - slab_front;

        let mut total = 0usize;
        let mut p = self.slab.get(slab_back)
            .map(|p| p as *const u8)
            .unwrap_or(ptr::null());
        while self.slab.as_ptr_range().contains(&p) {
            let e = unsafe { &*(p as *const Ebuf) };
            // the risky thing about this is we can end up with an
            // uninitialized ebuf here, which can be problematic
            if self.ebuf_npw2(e) > npw2(self.slab.len()) {
                break;
            }

            total += 1;
            p = unsafe {
                p.add(alignup(
                    size_of::<Ebuf>() + (Eptr::ALIGN << self.ebuf_npw2(e)),
                    Eptr::ALIGN
                ))
            }
        }

        // find pending usage
        let mut pending = 0;
        let mut pending_bytes = 0;
        let mut slices = 0;
        let mut nhead = self.eptr_load(&self.queue);
        while let Some(head) = nhead {
            slices += 1;
            let mut nsibling = Some(head);
            while let Some(sibling) = nsibling {
                pending += 1;
                pending_bytes += size_of::<Ebuf>() + (Eptr::ALIGN << self.ebuf_npw2(sibling));
                nsibling = self.eptr_load(&sibling.sibling);
            }
            
            nhead = self.eptr_load(&head.next);
        }

        // find bucket usage
        let buckets = self.buckets();
        let mut free = 0;
        let mut free_bytes = 0;
        for (npw2, mut eptr) in buckets.iter().enumerate() {
            while let Some(e) = self.eptr_load(eptr) {
                free += 1;
                free_bytes += size_of::<Ebuf>() + (Eptr::ALIGN << npw2);
                eptr = &e.sibling;
            }
        }

        // these are best-effort numbers, it's very easy for in-flight events
        // to create numbers that are just flat-out wrong
        //
        // we can at least clamp some of the numbers to reasonable limits
        // to avoid breaking user's code as much as possible
        let pending = min(pending, total);
        let pending_bytes = min(pending_bytes, slab_total.saturating_sub(slab_unused+slab_front));

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
        for (bucket, mut eptr) in buckets.iter_mut().zip(self.buckets()) {
            let mut count = 0;
            while let Some(e) = self.eptr_load(eptr) {
                count += 1;
                eptr = &e.sibling;
            }
            *bucket = count;
        }
    }

    pub fn slice_usage(&self, slices: &mut [usize]) {
        let mut slices_iter = slices.iter_mut();
        let mut nhead = self.eptr_load(&self.queue);
        while let Some((slice, head)) = slices_iter.next().zip(nhead) {
            let mut count = 0;
            let mut nsibling = Some(head);
            while let Some(sibling) = nsibling {
                count += 1;
                nsibling = self.eptr_load(&sibling.sibling);
            }

            *slice = count;
            nhead = self.eptr_load(&head.next);
        }
    }
}



