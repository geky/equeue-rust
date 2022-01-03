
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
/// The atomic double-word unit used in equeue
#[allow(non_camel_case_types)]
pub(crate) type udeptr = u64;
#[allow(non_camel_case_types)]
pub(crate) type ideptr = i64;

/// Integer that fits an in-slab equeue pointer, should be 1/2 of a udeptr
#[allow(non_camel_case_types)]
pub(crate) type ueptr = u32;
#[allow(non_camel_case_types)]
pub(crate) type ieptr = i32;

pub(crate) type NonZeroUeptr = NonZeroU32;

/// Integer that fits a pointer generation count, should be 1/4 of a udeptr
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
pub(crate) struct AtomicUdeptr(AtomicU64);

impl AtomicU for AtomicUdeptr {
    type U = udeptr;

    #[inline]
    fn new(v: udeptr) -> Self {
        Self(AtomicU64::new(v))
    }

    /// Atomic load
    #[inline]
    fn load(&self) -> udeptr {
        self.0.load(atomic::Ordering::SeqCst)
    }

    /// Atomic store, must always be inside a critical section
    #[inline]
    fn store(&self, v: udeptr) {
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


// Locking primitive
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


// Semaphore primitive
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
