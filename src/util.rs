
// TODO feature gate this?
use core::sync::atomic::AtomicUsize;
use core::sync::atomic::Ordering;
use core::marker::PhantomData;
use core::mem::size_of;

use std::time::Instant;
use std::time::Duration;
use std::sync::Mutex;
use std::sync::Condvar;


#[inline]
pub(crate) const fn max(a: usize, b: usize) -> usize {
    if a > b { a } else { b }
}

#[inline]
pub(crate) const fn min(a: usize, b: usize) -> usize {
    if a < b { a } else { b }
}

#[inline]
pub(crate) const fn aligndown(a: usize, align: usize) -> usize {
    a - (a % align)
}

#[inline]
pub(crate) const fn alignup(a: usize, align: usize) -> usize {
    aligndown(a + align-1, align)
}

#[inline]
pub(crate) const fn npw2(a: usize) -> u8 {
    a.next_power_of_two().trailing_zeros() as u8
}

// TODO these should be wrapped more flexbily
// - allow overriding
// - allow mutex/disable_irq impls?
// - allow type overriding?

// Atomic primitives
#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct Cas<T>(AtomicUsize, PhantomData<T>);

impl<T> Cas<T> {
    pub fn new(t: T) -> Self {
        debug_assert_eq!(size_of::<T>(), size_of::<usize>());
        Self(
            AtomicUsize::new(unsafe { *(&t as *const _ as *const usize) }),
            PhantomData
        )
    }

    /// Atomic load
    pub fn load(&self) -> T where T: Copy {
        unsafe { *(&self.0.load(Ordering::SeqCst) as *const _ as *const T) }
    }

    /// Atomic compare-and-swap
    pub fn cas(&self, old: T, new: T) -> Result<T, T> where T: Copy {
        unsafe { 
            *(&self.0.compare_exchange_weak(
                *(&old as *const _ as *const usize),
                *(&new as *const _ as *const usize),
                Ordering::SeqCst,
                Ordering::SeqCst
            ) as *const _ as *const Result<T, T>)
        }
    }

    /// Non-atomic load iff we have exclusive access, we can
    /// leverage Rust's mut for this
    pub fn load_ex(&mut self) -> T where T: Copy {
        unsafe { *(&self.0.load(Ordering::Relaxed) as *const _ as *const T) }
    }

    /// Non-atomic store iff we have exclusive access, we can
    /// leverage Rust's mut for this
    pub fn store_ex(&mut self, new: T) {
        self.0.store(
            unsafe { *(&new as *const _ as *const usize) },
            Ordering::Relaxed
        )
    }
}

// Time primitives
#[allow(non_camel_case_types)]
pub(crate) type utick = u64;

#[allow(non_camel_case_types)]
pub(crate) type itick = i64;

#[derive(Debug)]
pub(crate) struct Clock(Instant);

pub(crate) fn scmp(a: utick, b: utick) -> itick {
    a.wrapping_sub(b) as itick
}

impl Clock {
    pub fn new() -> Clock {
        Clock(Instant::now())
    }

    pub fn now(&self) -> utick {
        Instant::now()
            .duration_since(self.0)
            .as_millis()
            as utick
    }
}

// Semaphore primitives
#[derive(Debug)]
pub(crate) struct Sema {
    mutex: Mutex<()>,
    cond: Condvar,
}

impl Sema {
    pub fn new() -> Sema {
        Sema {
            mutex: Mutex::new(()),
            cond: Condvar::new()
        }
    }

    pub fn signal(&self) {
        self.cond.notify_one();
    }

    pub fn wait(&self, ticks: itick) {
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
