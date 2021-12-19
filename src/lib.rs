
// TODO enable
//#![no_std]

#![deny(missing_debug_implementations)]

// TODO remove this eventually
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use core::mem::transmute_copy;
use core::mem::size_of;
use core::mem::align_of;
use core::fmt;
use core::alloc::Layout;
use core::ptr;
use core::slice;
use core::ops::Range;

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

/// Internal event header
#[derive(Debug)]
struct Ebuf {
    sibling: Atomic<Eptr>,
    npw2: u8,
}

impl Ebuf {
    unsafe fn from_data(ptr: *mut u8) -> Option<&'static mut Ebuf> {
        if !ptr.is_null() {
            Some(&mut *(ptr as *mut Ebuf).sub(1))
        } else {
            None
        }
    }

    unsafe fn data(&mut self) -> *mut u8 {
        (self as *mut Ebuf).add(1) as *mut u8
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
}


/// Event queue struct
#[derive(Debug)]
pub struct Equeue {
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

    fn eptr_from(&self, e: &Ebuf) -> Eptr {
        unsafe {
            Eptr(
                (e as *const Ebuf as *const u8)
                    .offset_from(self.slab.as_ptr())
                    as usize
            )
        }
    }

    fn eptr_eq(&self, a: Eptr, b: Eptr) -> bool {
        let mask = (1 << self.npw2) - 1;
        a.0 & mask == b.0 & mask
    }

    fn eptr_ref<'a>(&'a self, eptr: Eptr) -> Option<&'a Ebuf> {
        let mask = (1 << self.npw2) - 1;
        if eptr.0 & mask != 0 {
            Some(unsafe { &*(&self.slab[eptr.0 & mask] as *const u8 as *const Ebuf) }) 
        } else {
            None
        }
    }

    fn eptr_mut<'a>(&'a self, eptr: Eptr) -> Option<&'a mut Ebuf> {
        unsafe { self.eptr_ref(eptr).map(|eptr| &mut *(eptr as *const Ebuf as *mut Ebuf)) }
    }

    fn eptr_generation(&self, eptr: Eptr) -> usize {
        eptr.0 >> self.npw2
    }

    fn eptr_inc(&self, old: Eptr, new: Eptr) -> Eptr {
        let mask = (1 << self.npw2) - 1;
        Eptr((old.0.wrapping_add(1 << self.npw2) & !mask) | (new.0 & mask))
    }

    fn ebuf_alloc<'a>(&'a self, layout: Layout) -> Result<&'a mut Ebuf, Error> {
        assert!(layout.align() <= Eptr::ALIGN);

        // find best bucket
        let npw2 = npw2((layout.size()+Eptr::ALIGN-1) / Eptr::ALIGN);

        'retry: loop {
            // first, do we have an allocation in our buckets? we don't look
            // at larger buckets because those are likely to be reused, we don't
            // want to starve larger events with smaller events
            if let Some(bucket) = self.buckets().get(npw2 as usize) {
                let eptr = bucket.load();
                if let Some(e) = self.eptr_ref(eptr) {
                    // CAS try to take an event from a bucket
                    let sibling = e.sibling.load();
                    if let Err(_) = bucket.cas(eptr,
                        self.eptr_inc(eptr, sibling))
                    {
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
                });

                return Ok(&mut *e);
            }
        }
    }

    fn ebuf_dealloc(&self, e: &mut Ebuf) {
        debug_assert!(self.slab.as_ptr_range()
            .contains(&(e as *const _ as *const u8)));

        // we only ever need to load buckets once because it can never shrink
        let bucket = &self.buckets()[e.npw2 as usize];

        'retry: loop {
            // try to insert into our bucket
            let sibling = bucket.load();
            debug_assert!(!self.eptr_eq(sibling, self.eptr_from(e)));
            e.sibling.store_ex(sibling);
            // CAS try to add our event to a bucket
            if let Err(_) = bucket.cas(sibling,
                self.eptr_inc(sibling, self.eptr_from(e)))
            {
                continue 'retry;
            }

            return;
        }
    }

    pub unsafe fn alloc_raw(&self, layout: Layout) -> *mut u8 {
        match self.ebuf_alloc(layout) {
            Ok(e) => e.data(),
            Err(_) => ptr::null_mut(),
        }
    }

    pub unsafe fn dealloc_raw(&self, ptr: *mut u8, _layout: Layout) {
        let e = match Ebuf::from_data(ptr) {
            Some(e) => self.ebuf_dealloc(e),
            None => return, // do nothing
        };
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
            while let Some(e) = self.eptr_ref(eptr.load()) {
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
            while let Some(e) = self.eptr_ref(eptr.load()) {
                count += 1;
                eptr = &e.sibling;
            }
            *bucket = count;
        }
    }
}



