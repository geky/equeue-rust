
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
    const ALIGN: usize = max(
        max(align_of::<Eptr>(), align_of::<Ebuf>()),
        align_of::<*const usize>()
    );

    const fn null() -> Eptr {
        Eptr(0)
    }
}

impl Equeue {
    // Eptr interactions
    fn eptr_from<'a>(
        &'a self,
        mark: usize,
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

        // make sure our mark/gen/off aren't overflowing
        debug_assert!(mark <= (1 << self.gnpw2));
        debug_assert!(gen <= (1 << self.gnpw2));
        debug_assert!(off <= (1 << self.enpw2));

        Eptr((mark << self.gnpw2+self.enpw2) | (gen << self.enpw2) | off)
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
}

/// Internal event header
#[derive(Debug)]
struct Ebuf {
    npw2: u8,
    next: Cas<Eptr>,
    sibling: Cas<Eptr>,

    cb: Option<fn(*mut u8)>,
    drop: Option<fn(*mut u8)>,
    target: utick,
}

impl Ebuf {
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
    fn ebuf_gen_load<'a>(&'a self, e: &'a Ebuf) -> usize {
        self.eptr_mark(e.next.load())
    }

    fn ebuf_gen_load_ex<'a>(&'a self, e: &'a mut Ebuf) -> usize {
        self.eptr_mark(e.next.load_ex())
    }

    fn ebuf_gen_inc_ex<'a>(&'a self, e: &'a mut Ebuf) {
        let next = self.eptr_inc_mark(e.next.load_ex());
        e.next.store_ex(next);
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

        loop {
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

                    return Ok(unsafe { self.ebuf_claim(e) });
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
                    continue;
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
                continue;
            }

            unsafe {
                let e = &self.slab[new_slab_back] as *const u8 as *const Ebuf as *mut Ebuf;
                e.write(Ebuf {
                    next: Cas::new(Eptr::null()),
                    sibling: Cas::new(Eptr::null()),
                    npw2: npw2,

                    cb: None,
                    drop: None,
                    target: 0,
                });

                return Ok(&mut *e);
            }
        }
    }

    fn dealloc_ebuf(&self, e: &mut Ebuf) {
        debug_assert!(self.contains_ebuf(e));

        // we have exclusive access so generation can't change
        let gen = self.ebuf_gen_load_ex(e);
        // we only ever need to load buckets once because it can never shrink
        let bucket = &self.buckets()[e.npw2 as usize];

        // CAS try to add our event to a bucket
        let mut siblingptr = bucket.load();
        loop {
            debug_assert_ne!(e as *const _, self.eptr_as_ptr(siblingptr));
            e.sibling.store_ex(siblingptr);

            let eptr = self.eptr_from(0, gen, Some(e));
            if let Err(x) = bucket.cas(siblingptr, eptr) {
                siblingptr = x;
                continue;
            }

            return;
        }
    }

    // Queue management
    fn enqueue_ebuf(&self, e: &mut Ebuf, target: utick) {
        debug_assert!(e.cb.is_some());
        e.target = target;

        // we have exclusive access so generation can't change
        let gen = self.ebuf_gen_load_ex(e);

        'retry: loop {
            let sibling = self.queue.load();

            // if someone is trying to remove the head we need to help out
            if self.eptr_mark(sibling) != 0 {
                self.help_dequeue_ebufs_1(
                    // we should only find a dirty head if head is non-null
                    sibling, self.eptr_as_ref(sibling).unwrap()
                );
                continue 'retry;
            }

            // CAS try to insert our event into the queue
            debug_assert_ne!(e as *const _, self.eptr_as_ptr(sibling));
            e.sibling.store_ex(sibling);
            e.next.store_ex(
                self.eptr_set_mark(
                    match self.eptr_as_ref(sibling) {
                        Some(sibling) => sibling.next.load(),
                        None => Eptr::null(),
                    },
                    gen
                )
            );

            let eptr = self.eptr_from(0, gen, Some(e));
            if let Err(_) = self.queue.cas(sibling, eptr) {
                continue 'retry;
            }

            return;
        }
    }

    // did we find a dirty head?
    //
    // stop being lazy and help complete the remove
    //
    fn help_dequeue_ebufs_1<'a>(
        &'a self,
        dirty_headptr: Eptr,
        head: &'a Ebuf
    ) -> Eptr {
        // CAS 2. increment our generation count, which is embedded in
        // our next-pointer, this prevents any in-flight insertions
        // from update our next pointer
        //
        // note sibling pointers are never updated once an event is
        // shared!
        let gen = self.eptr_gen(dirty_headptr);
        let mut nextptr = head.next.load();
        let mut dirty_nextptr = nextptr;
        while self.eptr_mark(nextptr) == gen {
            dirty_nextptr = self.eptr_inc_mark(nextptr);
            if let Err(x) = head.next.cas(nextptr, dirty_nextptr) {
                nextptr = x;
                continue;
            }

            break;
        }

        // go on to finish the remove
        self.help_dequeue_ebufs_2(dirty_headptr, dirty_nextptr)
    }

    // did we find a dirty next pointer?
    //
    // stop being lazy and help complete the remove
    //
    fn help_dequeue_ebufs_2<'a>(
        &'a self,
        dirty_headptr: Eptr,
        dirty_nextptr: Eptr
    ) -> Eptr {
        // CAS 3. actually remove the slice
        //
        // at this point, we know the queue head must be dirty, so ANY
        // update must be removing the head slice, so if our cas fails
        // we can assume the slice has been removed
        let nextptr = self.eptr_set_mark(dirty_nextptr, 0);
        self.queue.cas(dirty_headptr, nextptr)
            .unwrap_or_else(|x| x)
    }

    fn dequeue_ebufs<'a>(&'a self, now: utick) -> Result<&'a mut Ebuf, itick> {
        let (dirty_headptr, head) = {
            let mut headptr = self.queue.load();
            loop {
                if let Some(head) = self.eptr_as_ref(headptr) {
                    // if someone is trying to remove the head we need to help out
                    if self.eptr_mark(headptr) != 0 {
                        headptr = self.help_dequeue_ebufs_1(headptr, head);
                        continue;
                    }

                    // is this slice ready to dispatch?
                    //
                    // TODO does this prevent us from runnin multiple dispatches
                    // simultaneously? not that we need to support this, just that
                    // we should know what's possible
                    let delta = scmp(head.target, now);
                    if delta > 0 {
                        return Err(delta);
                    }

                    // CAS 1. mark our slice for removal, this prevents any new
                    // events from being inserted in front of us
                    let dirty_head = self.eptr_set_mark(headptr, 1);
                    if let Err(x) = self.queue.cas(headptr, dirty_head) {
                        headptr = x;
                        continue;
                    }

                    break (dirty_head, head);
                } else {
                    // nothing to do I guess
                    return Err(-1);
                }
            }
        };

        // try to finish the remove, note that other threads may actually
        // finish the remove for us, which is why these are separate
        // functions
        self.help_dequeue_ebufs_1(dirty_headptr, head);

        // at this point we should have exclusive access to the slice!
        let mut head = unsafe { self.ebuf_claim(head) };

        // Our events are just pushed on the top of the queue, creating a set
        // of linked-list stacks. We need to reverse and flatten each slice in
        // order to match insertion order
        let mut prev = None;
        while let Some(sibling) = self.eptr_load_ex(&mut head.sibling) {
            head.sibling.store_ex(self.eptr_from(0, 0, prev));
            prev = Some(head);
            head = sibling;

            // for every non-head event we need to update the generation,
            // fortunately we don't have to worry about shared access
            self.ebuf_gen_inc_ex(head);
        }
        head.sibling.store_ex(self.eptr_from(0, 0, prev));

        Ok(head)
    }

    // Central dispatch function
    pub fn dispatch(&self, ticks: itick) {
        // get the current time
        let now = self.clock.now();

        // get a slice to dispatch
        let mut slice = self.dequeue_ebufs(now).ok();
        while let Some(e) = slice {
            // dispatch!
            e.cb.unwrap()(unsafe { e.as_mut_ptr() });

            // move to next event
            slice = self.eptr_load_ex(&mut e.sibling);

            // call drop, return event to memory pool
            if let Some(drop) = e.drop {
                drop(unsafe { e.as_mut_ptr() });
            }
            self.dealloc_ebuf(e);
        }
    }

    // Handling of raw allocations
    pub unsafe fn alloc_raw(&self, layout: Layout) -> *mut u8 {
        match self.alloc_ebuf(layout) {
            Ok(e) => {
                e.drop = None;
                e.target = 0;
                e.as_mut_ptr()
            }
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

    pub unsafe fn post_raw(&self, cb: fn(*mut u8), e: *mut u8) {
        let mut e = Ebuf::from_mut_ptr(e).unwrap();
        debug_assert!(self.contains_ebuf(e));
        e.cb = Some(cb);
        self.enqueue_ebuf(e, self.clock.now() + e.target);
    }
}


/// Post trait
pub trait Post {
    fn post(&mut self);
}

impl<F: FnMut() + Send> Post for F {
    fn post(&mut self) {
        self()
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
        e.drop = None;
        e.target = 0;
        unsafe { e.as_mut_ptr::<T>().write(T::default()); }
        Ok(Event::from_ebuf(self, e))
    }

    pub fn alloc_from<'a, T>(&'a self, t: T) -> Result<Event<'a, T>, Error> {
        let e = self.alloc_ebuf(Layout::new::<T>())?;
        e.drop = None;
        e.target = 0;
        unsafe { e.as_mut_ptr::<T>().write(t); }
        Ok(Event::from_ebuf(self, e))
    }

    pub fn post<T: Post>(&self, e: Event<'_, T>) {
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
        e.q.enqueue_ebuf(e.e, self.clock.now() + e.e.target);
        forget(e);
    }
}

impl<T: Post> Event<'_, T> {
    pub fn delay(self, delay: itick) -> Self {
        debug_assert!(delay >= 0);
        self.e.target = delay as utick;
        self
    }

    pub fn post(self) {
        self.q.post(self);
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
    pub fn call<F: Post>(&self, cb: F) -> Result<(), Error>{
        self.alloc_from(cb)?
            .post();
        Ok(())
    }

    pub fn call_in<F: Post>(&self, delay: itick, cb: F) -> Result<(), Error> {
        self.alloc_from(cb)?
            .delay(delay)
            .post();
        Ok(())
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
            if e.npw2 > self.enpw2 {
                break;
            }

            total += 1;
            p = unsafe {
                p.add(alignup(
                    size_of::<Ebuf>() + (Eptr::ALIGN << e.npw2),
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
                pending_bytes += size_of::<Ebuf>() + (Eptr::ALIGN << sibling.npw2);
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
        let pending_bytes = min(pending_bytes, slab_total);

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



