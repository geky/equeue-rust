
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
            transmute_copy::<_, T>(&aload(self as *const _ as *const usize))
        }
    }

    /// Atomic compare-and-swap
    fn cas(&self, old: T, new: T) -> Result<T, T> {
        unsafe {
            transmute_copy::<_, Result<T,  T>>(
                &acas(
                    self as *const _ as *const usize,
                    transmute_copy::<_, usize>(&old),
                    transmute_copy::<_, usize>(&new),
                )
            )
        }
    }

    /// Non-atomic load iff we have exclusive access
    unsafe fn load_ex(&self, new: T) -> T {
        (self as *const _ as *const T).read()
    }

    /// Non-atomic store iff we have exclusive access
    unsafe fn store_ex(&mut self, new: T) {
        (self as *mut _ as *mut T).write(new);
    }
}


/// Internal event struct
#[derive(Debug)]
struct Ev {
    sibling: Atomic<*const Ev>,
    npw2: u8,
}

/// Event queue struct
#[derive(Debug)]
pub struct Equeue {
    slab: Range<*const u8>,
    slab_front: Atomic<*const u8>,
    slab_back: Atomic<*const u8>,
}

unsafe impl Send for Equeue {}
unsafe impl Sync for Equeue {}

impl Equeue {
    pub fn with_buffer(buffer: &'static mut [u8]) -> Result<Equeue, Error> {
        let buffer_range = buffer.as_mut_ptr_range();
        // align front to nearest bucket
        let slab_front = alignup(
            buffer_range.start as usize,
            align_of::<Option<&'static mut Ev>>()
        ) as *const u8;
        let slab_back = buffer_range.end;

        // do we already overlap?
        if slab_front > slab_back {
            return Err(Error::NoMem);
        }

        // go ahead and zero our buffer, this makes it easier to manage bucket
        // allocation, which needs to be null the moment a bucket is allocated
        unsafe {
            slice::from_raw_parts_mut(
                slab_front as *mut u8,
                slab_back.offset_from(slab_front) as usize
            )
            .fill(0);
        }

        Ok(Equeue {
            slab: slab_front .. slab_back,
            slab_front: Atomic::new(slab_front),
            slab_back: Atomic::new(slab_back),
        })
    }

    fn load_buckets(&self) -> &[Atomic<*const Ev>] {
        let slab_start = self.slab.start as *const Atomic<*const Ev>;
        let slab_front = self.slab_front.load() as *const Atomic<*const Ev>;
        unsafe {
            slice::from_raw_parts(
                slab_start,
                slab_front.offset_from(slab_start) as usize
            )
        }
    }

    pub unsafe fn alloc_raw(&self, layout: Layout) -> *mut u8 {
        // TODO handle larger align?
        assert!(layout.align() <= align_of::<Ev>());

        // find best bucket
        let npw2 = util::npw2(layout.size());
        let bucket = npw2.checked_sub(util::npw2(align_of::<Ev>()))
            .unwrap_or(0) as usize;

        'retry: loop {
            let slab_front = self.slab_front.load();
            let slab_back = self.slab_back.load();

            // first, do we have an allocation in our buckets? we don't look
            // at larger buckets because those are likely to be reused, we don't
            // want to starve larger events with smaller events
            if let Some(bucket) = self.load_buckets().get(bucket) {
                let e = bucket.load();
                if let Some(e) = e.as_ref() {
                    // CAS try to take an event from a bucket
                    let sibling = e.sibling.load();
                    if let Err(_) = bucket.cas(e, sibling) {
                        continue 'retry;
                    }

                    // get pointer to trailing data
                    return (e as *const Ev).add(1) as *const u8 as *mut u8;
                }
            }

            // second, a litmus test, do we even have enough memory to satisfy
            // our request?
            let new_slab_front = max(
                slab_front,
                (self.slab.start as *const Atomic<*const Ev>).add(bucket + 1) as *const u8
            );
            let new_slab_back = aligndown(
                slab_back as usize - (size_of::<Ev>() + (1 << npw2)),
                align_of::<Ev>()
            ) as *const u8;

            if new_slab_front > new_slab_back {
                return ptr::null_mut();
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

            let e = new_slab_back as *const Ev as *mut Ev;
            e.write(Ev {
                sibling: Atomic::new(ptr::null()),
                npw2: npw2,
            });

            // get pointer to trailing data
            return (e as *const Ev).add(1) as *const u8 as *mut u8;
        }
    }

    pub unsafe fn dealloc_raw(&self, ptr: *mut u8, layout: Layout) {
        if ptr.is_null() {
            return;
        }

        // get access to the event metadata
        debug_assert!(self.slab.contains(&(ptr as *const _)));
        let e = &mut *(ptr as *mut Ev).sub(1);

        // find best bucket
        let bucket = e.npw2.checked_sub(util::npw2(align_of::<Ev>()))
            .unwrap_or(0) as usize;

        // we only ever need to load this once because it can never decrease
        let bucket = &self.load_buckets()[bucket];
        'retry: loop {
            let sibling = bucket.load();
            debug_assert_ne!(sibling, e);
            e.sibling.store_ex(sibling);
            // CAS try to add our event to a bucket
            if let Err(_) = bucket.cas(sibling, e) {
                continue 'retry;
            }

            return;
        }
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
        unsafe {
            let slab_front = self.slab_front.load();
            let slab_back = self.slab_back.load();
            let buckets = self.load_buckets();

            let slab_total = self.slab.end.offset_from(self.slab.start) as usize;
            let slab_used = slab_total - (slab_back.offset_from(slab_front) as usize);

            let mut slab_fragmented = 0;
            for (i, mut e_ptr) in buckets.iter().enumerate() {
                let npw2 = (i as u8) + util::npw2(align_of::<Ev>());
                while let Some(e) = e_ptr.load().as_ref() {
                    slab_fragmented += size_of::<Ev>() + (1 << npw2);
                    e_ptr = &e.sibling;
                }
            }

            Usage {
                slab_total: slab_total,
                slab_used: slab_used - slab_fragmented,
                slab_fragmented: slab_fragmented,
                buckets: buckets.len(),
            }
        }
    }

    pub fn bucket_usage(&self, buckets: &mut [usize]) {
        unsafe {
            for (bucket, mut e_ptr) in
                buckets.iter_mut()
                    .zip(self.load_buckets().iter())
            {
                let mut count = 0;
                while let Some(e) = e_ptr.load().as_ref() {
                    count += 1;
                    e_ptr = &e.sibling;
                }
                *bucket = count;
            }
        }
    }
}



