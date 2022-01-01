
// TODO feature gate this?
use core::sync::atomic::AtomicU64;
use core::sync::atomic::AtomicU32;
use core::sync::atomic::AtomicU16;
use core::sync::atomic::AtomicBool;
use core::num::NonZeroU32;
use core::sync::atomic::fence;
use core::sync::atomic;
use core::marker::PhantomData;
use core::mem::size_of;
use core::cmp::Ordering;
use core::fmt::Debug;

use std::time::Instant;
use std::time::Duration;
use std::sync::Mutex;
use std::sync::Condvar;


// TODO these should be wrapped more flexbily
// - allow overriding
// - allow mutex/disable_irq impls?
// - allow type overriding?

// Time primitives
#[allow(non_camel_case_types)]
pub(crate) type utick = u64;
#[allow(non_camel_case_types)]
pub(crate) type itick = i64;

pub(crate) trait Clock: Send + Sync {
    fn new() -> Self;
    fn now(&self) -> utick;
}

#[derive(Debug)]
pub(crate) struct DefaultClock(Instant);

impl Clock for DefaultClock {
    fn new() -> Self {
        Self(Instant::now())
    }

    fn now(&self) -> utick {
        Instant::now()
            .duration_since(self.0)
            .as_millis()
            as utick
    }
}

// Atomic primitives

// TODO actually adjust these based on target_width + feature flags
/// The atomic unit used in equeue
#[allow(non_camel_case_types)]
pub(crate) type uatom = u64;
#[allow(non_camel_case_types)]
pub(crate) type iatom = i64;

/// Integer that fits an in-slab equeue pointer, should be 1/2 of a uatom
#[allow(non_camel_case_types)]
pub(crate) type ueptr = u32;
#[allow(non_camel_case_types)]
pub(crate) type ieptr = i32;

pub(crate) type NonZeroUeptr = NonZeroU32;

/// Integer that fits a pointer generation count, should be 1/4 of a uatom
#[allow(non_camel_case_types)]
pub(crate) type ugen = u16;
#[allow(non_camel_case_types)]
pub(crate) type igen = i32;


// we really only need loads and stores, and modifications to any
// shared variables always occur in critical sections (via Lock)
pub(crate) trait AtomicU {
    type U: Copy;
    fn new(v: Self::U) -> Self;
    fn load(&self) -> Self::U;
    fn store(&self, v: Self::U);
}

pub(crate) use core::sync::atomic::AtomicUsize;

impl AtomicU for AtomicUsize {
    type U = usize;

    #[inline]
    fn new(v: usize) -> Self {
        AtomicUsize::new(v)
    }

    /// Atomic load
    #[inline]
    fn load(&self) -> usize {
        self.load(atomic::Ordering::SeqCst)
    }

    /// Atomic store, must always be inside a critical section
    #[inline]
    fn store(&self, v: usize) {
        self.store(v, atomic::Ordering::SeqCst)
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct AtomicUatom(AtomicU64);

impl AtomicU for AtomicUatom {
    type U = uatom;

    #[inline]
    fn new(v: uatom) -> Self {
        Self(AtomicU64::new(v))
    }

    /// Atomic load
    #[inline]
    fn load(&self) -> uatom {
        self.0.load(atomic::Ordering::SeqCst)
    }

    /// Atomic store, must always be inside a critical section
    #[inline]
    fn store(&self, v: uatom) {
        self.0.store(v, atomic::Ordering::SeqCst)
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct AtomicUeptr(AtomicU32);

impl AtomicU for AtomicUeptr {
    type U = ueptr;

    #[inline]
    fn new(v: ueptr) -> Self {
        Self(AtomicU32::new(v))
    }

    /// Atomic load
    #[inline]
    fn load(&self) -> ueptr {
        self.0.load(atomic::Ordering::SeqCst)
    }

    /// Atomic store, must always be inside a critical section
    #[inline]
    fn store(&self, v: ueptr) {
        self.0.store(v, atomic::Ordering::SeqCst)
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct AtomicUgen(AtomicU16);

impl AtomicU for AtomicUgen {
    type U = ugen;

    #[inline]
    fn new(v: ugen) -> Self {
        Self(AtomicU16::new(v))
    }

    /// Atomic load
    #[inline]
    fn load(&self) -> ugen {
        self.0.load(atomic::Ordering::SeqCst)
    }

    /// Atomic store, must always be inside a critical section
    #[inline]
    fn store(&self, v: ugen) {
        self.0.store(v, atomic::Ordering::SeqCst)
    }
}

//#[derive(Debug)]
//#[repr(transparent)]
//pub(crate) struct Cas<T>(AtomicUsize, PhantomData<T>);
//
//impl<T> Cas<T> {
//    pub fn new(t: T) -> Self {
//        debug_assert_eq!(size_of::<T>(), size_of::<usize>());
//        Self(
//            AtomicUsize::new(unsafe { *(&t as *const _ as *const usize) }),
//            PhantomData
//        )
//    }
//
//    /// Atomic load
//    pub fn load(&self) -> T where T: Copy {
//        unsafe { *(&self.0.load(atomic::Ordering::SeqCst) as *const _ as *const T) }
//    }
//
//    /// Atomic compare-and-swap
//    pub fn cas(&self, old: T, new: T) -> Result<T, T> where T: Copy {
//        unsafe { 
//            //*(&self.0.compare_exchange_weak(
//            *(&self.0.compare_exchange(
//                *(&old as *const _ as *const usize),
//                *(&new as *const _ as *const usize),
//                atomic::Ordering::SeqCst,
//                atomic::Ordering::SeqCst
//            ) as *const _ as *const Result<T, T>)
//        }
//    }
//
//    /// Non-atomic load iff we have exclusive access, we can
//    /// leverage Rust's mut for this
//    pub fn load_ex(&mut self) -> T where T: Copy {
//        unsafe { *(&self.0.load(atomic::Ordering::Relaxed) as *const _ as *const T) }
//    }
//
//    /// Non-atomic store iff we have exclusive access, we can
//    /// leverage Rust's mut for this
//    pub fn store_ex(&mut self, new: T) {
//        self.0.store(
//            unsafe { *(&new as *const _ as *const usize) },
//            // atomic::Ordering::Relaxed TODO
//            atomic::Ordering::SeqCst
//        )
//    }
//}

pub(crate) trait Lock: Send + Sync + Debug {
    fn new() -> Self;
    fn lock<R, F: FnOnce()->R>(&self, f: F) -> R;
}

#[derive(Debug)]
pub(crate) struct DefaultLock(Mutex<()>);

impl Lock for DefaultLock {
    fn new() -> Self {
        DefaultLock(Mutex::new(()))
    }

    fn lock<R, F: FnOnce() -> R>(&self, f: F) -> R {
        let guard = self.0.lock().unwrap();
        f()
    }
}

//#[derive(Debug)]
//pub(crate) struct DefaultLock(AtomicBool);
//
//impl Lock for DefaultLock {
//    fn new() -> Self {
//        DefaultLock(AtomicBool::new(false))
//    }
//
//    fn lock<R, F: FnOnce() -> R>(&self, f: F) -> R {
//        while !self.0.compare_exchange(
//            false, 
//            true,
//            atomic::Ordering::Acquire,
//            atomic::Ordering::Relaxed
//        ).is_ok() {}
//        let r = f();
//        self.0.store(false, atomic::Ordering::Release);
//        r
//    }
//}


// Semaphore primitives
pub(crate) trait Sema: Send + Sync + Debug {
    fn new() -> Self;
    fn signal(&self);
    fn wait(&self, ticks: itick);
}

#[derive(Debug)]
pub(crate) struct DefaultSema {
    mutex: Mutex<()>,
    cond: Condvar,
}

impl Sema for DefaultSema {
    fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
            cond: Condvar::new()
        }
    }

    fn signal(&self) {
        self.cond.notify_one();
    }

    fn wait(&self, ticks: itick) {
        let guard = self.mutex.lock().unwrap();
        if ticks < 0 {
            let _ = self.cond
                .wait(guard)
                .unwrap();
        } else {
            let _ = self.cond
                .wait_timeout(guard, Duration::from_millis(ticks as u64))
                .unwrap();
        }
    }
}
