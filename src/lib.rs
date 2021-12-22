
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


/// A type-safe wrapper around usized compare-and-swaps
#[derive(Debug)]
#[repr(transparent)]
struct Atomic<T>(T);

impl<T> Atomic<T> {
    fn new(t: T) -> Self {
        debug_assert_eq!(size_of::<T>(), size_of::<usize>());
        Self(t)
    }

    /// Atomic load
    fn load(&self) -> T {
        unsafe {
            (
                &aload(self as *const _ as *const usize)
                    as *const _ as *const T
            ).read()
        }
    }

    /// Atomic compare-and-swap
    fn cas(&self, old: T, new: T) -> Result<T, T> {
        unsafe {
            (
                &acas(
                    self as *const _ as *const usize,
                    *(&old as *const _ as *const usize),
                    *(&new as *const _ as *const usize),
                ) as *const _ as *const Result<T, T>
            ).read()
        }
    }

    /// Non-atomic load iff we have exclusive access
    fn load_ex(&mut self, new: T) -> T {
        unsafe { (self as *mut _ as *mut T).read() }
    }

    /// Non-atomic store iff we have exclusive access
    fn store_ex(&mut self, new: T) {
        unsafe { (self as *mut _ as *mut T).write(new) };
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
    const ALIGN: usize = max(
        max(align_of::<Eptr>(), align_of::<Ebuf>()),
        align_of::<*const usize>()
    );

    const fn null() -> Eptr {
        Eptr(0)
    }

    fn from(q: &Equeue, e: &Ebuf) -> Eptr {
        unsafe {
            Eptr(
                (e as *const Ebuf as *const u8)
                    .offset_from(q.slab.as_ptr())
                    as usize
            )
        }
    }

    fn eq(self, q: &Equeue, other: Eptr) -> bool {
        let mask = (1 << q.npw2) - 1;
        self.0 & mask == other.0 & mask
    }

    fn as_ref<'a>(self, q: &'a Equeue) -> Option<&'a Ebuf> {
        let mask = (1 << q.npw2) - 1;
        if self.0 & mask != 0 {
            Some(unsafe { &*(&q.slab[self.0 & mask] as *const u8 as *const Ebuf) }) 
        } else {
            None
        }
    }

    fn as_mut<'a>(self, q: &'a Equeue) -> Option<&'a mut Ebuf> {
        unsafe { self.as_ref(q).map(|e| &mut *(e as *const Ebuf as *mut Ebuf)) }
    }

    fn generation(self, q: &Equeue) -> usize {
        self.0 >> q.npw2
    }

    fn inc(self, q: &Equeue, old: Eptr) -> Eptr {
        let mask = (1 << q.npw2) - 1;
        Eptr((self.0 & mask) | (old.0.wrapping_add(1 << q.npw2) & !mask))
    }
}

/// Internal event header
#[derive(Debug)]
struct Ebuf {
    npw2: u8,
    sibling: Atomic<Eptr>,

    cb: Option<fn(*mut u8)>,
    drop: Option<fn(*mut u8)>,
}

impl Ebuf {
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

/// Event queue struct
#[derive(Debug)]
pub struct Equeue {
    // queue management
    queue: Atomic<Eptr>,

    // memory management
    slab: &'static [u8],
    npw2: u8,
    slab_front: Atomic<usize>,
    slab_back: Atomic<usize>,
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

        Ok(Equeue {
            slab: buffer,
            npw2: npw2(buffer.len()),
            slab_front: Atomic::new(0),
            slab_back: Atomic::new(buffer.len()),

            queue: Atomic::new(Eptr::null()),
        })
    }

    fn buckets<'a>(&'a self) -> &'a [Atomic<Eptr>] {
        let slab_front = self.slab_front.load();
        unsafe {
            slice::from_raw_parts(
                self.slab.as_ptr() as *const Atomic<Eptr>,
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
            // first, do we have an allocation in our buckets? we don't look
            // at larger buckets because those are likely to be reused, we don't
            // want to starve larger events with smaller events
            if let Some(bucket) = self.buckets().get(npw2 as usize) {
                let eptr = bucket.load();
                if let Some(e) = eptr.as_ref(self) {
                    // CAS try to take an event from a bucket
                    let sibling = e.sibling.load();
                    if let Err(_) = bucket.cas(eptr, sibling.inc(self, eptr)) {
                        continue 'retry;
                    }

                    return Ok(unsafe { &mut *(e as *const _ as *mut Ebuf) });
                }
            }

            // second, a litmus test, do we even have enough memory to satisfy
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

            // third, make sure we have enough buckets allocated
            if new_slab_front > slab_front {
                // CAS to allocate buckets
                if let Err(_) = self.slab_front.cas(slab_front, new_slab_front) {
                    continue 'retry;
                }
            }

            // fourth, allocate our new event
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
                    sibling: Atomic::new(Eptr::null()),
                    npw2: npw2,

                    cb: None,
                    drop: None,
                });

                return Ok(&mut *e);
            }
        }
    }

    fn dealloc_ebuf(&self, e: &mut Ebuf) {
        debug_assert!(self.contains_ebuf(e));

        // we only ever need to load buckets once because it can never shrink
        let bucket = &self.buckets()[e.npw2 as usize];

        'retry: loop {
            // try to insert into our bucket
            let eptr = Eptr::from(self, e);
            let sibling = bucket.load();
            debug_assert!(!eptr.eq(self, sibling));
            e.sibling.store_ex(sibling);
            // CAS try to add our event to a bucket
            if let Err(_) = bucket.cas(sibling, eptr.inc(self, sibling)) {
                continue 'retry;
            }

            return;
        }
    }

    // Queue management
    fn enqueue_ebuf(&self, e: &mut Ebuf) {
        debug_assert!(e.cb.is_some());
        todo!()
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

    pub unsafe fn post_raw(&self, e: *mut u8, cb: fn(*mut u8)) {
        let mut e = Ebuf::from_mut_ptr(e).unwrap();
        debug_assert!(self.contains_ebuf(e));
        e.cb = Some(cb);
        self.enqueue_ebuf(e);
    }
}


/// Post trait
pub trait Post {
    fn post(&mut self);
}

impl<F: FnMut()> Post for F {
    fn post(&mut self) {
        self()
    }
}


// TODO unsized drop?
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
        e.q.enqueue_ebuf(e.e);
        forget(e);
    }
}

impl<T: Post> Event<'_, T> {
    fn post(self) {
        self.q.post(self);
    }
}

impl<T> Drop for Event<'_, T> {
    fn drop(&mut self) {
        // make sure we clean up if the event isn't dispatched
        unsafe { drop_in_place(self.e.as_mut_ptr::<T>()) };
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
    fn call<F: Post>(&self, cb: F) -> Result<(), Error>{
        self.alloc_from(cb)?
            .post();
        Ok(())
    }
}


#[derive(Debug)]
pub struct Usage {
    pub slab_total: usize,
    pub slab_used: usize,
    pub slab_fragmented: usize,
    pub buckets: usize,
}

impl Equeue {
    pub fn usage(&self) -> Usage {
        let slab_front = self.slab_front.load();
        let slab_back = self.slab_back.load();
        let buckets = self.buckets();

        let mut slab_fragmented = 0;
        for (npw2, mut eptr) in buckets.iter().enumerate() {
            while let Some(e) = eptr.load().as_ref(self) {
                slab_fragmented += size_of::<Ebuf>() + (Eptr::ALIGN << npw2);
                eptr = &e.sibling;
            }
        }

        Usage {
            slab_total: self.slab.len(),
            slab_used: self.slab.len() - (slab_back - slab_front) - slab_fragmented,
            slab_fragmented: slab_fragmented,
            buckets: buckets.len(),
        }
    }

    pub fn bucket_usage(&self, buckets: &mut [usize]) {
        for (bucket, mut eptr) in buckets.iter_mut().zip(self.buckets()) {
            let mut count = 0;
            while let Some(e) = eptr.load().as_ref(self) {
                count += 1;
                eptr = &e.sibling;
            }
            *bucket = count;
        }
    }
}



