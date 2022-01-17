
// TODO feature gate this?
use core::sync::atomic::AtomicU64;
use core::sync::atomic::AtomicU32;
use core::sync::atomic::AtomicU16;
use core::sync::atomic::AtomicBool;
use core::num::NonZeroU32;
use core::num::NonZeroI64;
use core::sync::atomic::fence;
use core::sync::atomic;
use core::marker::PhantomData;
use core::mem::size_of;
use core::cmp::Ordering;
use core::fmt::Debug;
use core::future::Future;
use core::pin::Pin;
use core::task::Poll;
use core::task::Context;
use core::task::Waker;
use core::mem::transmute;
use core::time::Duration;

use std::time::Instant;
use std::sync::Mutex;
use std::sync::MutexGuard;
use std::sync::Condvar;

use async_io::Timer;

use crate::traits::*;
use crate::Error;
use crate::Delta;


// TODO these should be wrapped more flexbily
// - allow overriding
// - allow mutex/disable_irq impls?
// - allow type overriding?


// Memory allocation, this is optional
pub(crate) use std::alloc::alloc;
pub(crate) use std::alloc::dealloc;


// Time primitives
#[allow(non_camel_case_types)]
pub(crate) type utick = u64;
#[allow(non_camel_case_types)]
pub(crate) type itick = i64;

pub(crate) type NonZeroItick = NonZeroI64;

impl TryFrom<Duration> for Delta {
    type Error = ();
    fn try_from(d: Duration) -> Result<Delta, ()> {
        itick::try_from(d.as_millis()).ok()
            .and_then(|ticks| Delta::new(ticks))
            .ok_or(())
    }
}

impl From<Delta> for Duration {
    fn from(d: Delta) -> Duration {
        Duration::from_millis(d.ticks() as u64)
    }
}


// Some way to get the time
#[derive(Debug)]
pub(crate) struct DefaultClock(Instant);

impl DefaultClock {
    pub(crate) fn new() -> Self {
        Self(Instant::now())
    }
}

impl Clock for DefaultClock {
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
#[derive(Debug)]
pub(crate) struct DefaultLock(Mutex<()>);

impl DefaultLock {
    pub(crate) fn new() -> Self {
        DefaultLock(Mutex::new(()))
    }
}

impl Lock for DefaultLock {
    // unfortunately we can't define types with lifetimes
    // in traits, the best we can do is unsafely strip the
    // lifetime and leave it up to the caller to drop the
    // types in the correct order
    type Guard = MutexGuard<'static, ()>;

    fn lock(&self) -> Self::Guard {
        // strip lifetime
        let guard = self.0.lock().unwrap();
        unsafe { transmute::<MutexGuard<'_, ()>, _>(guard) }
    }
}


// Semaphore primitive
#[derive(Debug)]
pub(crate) struct DefaultSema {
    mutex: Mutex<()>,
    cond: Condvar,
    waker: Mutex<Option<Waker>>,
}

impl DefaultSema {
    pub(crate) fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
            cond: Condvar::new(),
            waker: Mutex::new(None),
        }
    }
}

impl Sema for DefaultSema {
    fn signal(&self) {
        self.cond.notify_one();
    }

    fn wait(&self, delta: Option<Delta>) {
        let guard = self.mutex.lock().unwrap();
        if let Some(delta) = delta {
            let _ = self.cond
                .wait_timeout(guard, Duration::from_millis(delta.ticks() as u64))
                .unwrap();
        } else {
            let _ = self.cond
                .wait(guard)
                .unwrap();
        }
    }
}

#[derive(Debug)]
pub(crate) struct DefaultSemaAsyncWait<'a> {
    sema: &'a DefaultSema,
    timer: Option<Timer>,
}

impl Future for DefaultSemaAsyncWait<'_> {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // we're ok with spurious wakeups, so we can return as soon as we get
        // any wakeup, though this does mean we need to poll at least once
        let mut waker = self.sema.waker.lock().unwrap();
        if waker.is_some() {
            *waker = None;
            drop(waker);

            Poll::Ready(())
        } else {
            // save waker for signalling
            *waker = Some(cx.waker().clone());
            drop(waker);

            if let Some(ref mut timer) = self.timer {
                // wait on timer
                unsafe { Pin::new_unchecked(timer) }
                    .poll(cx)
                    .map(|_| ())
            } else {
                Poll::Pending
            }
        }
    }
}

impl Drop for DefaultSemaAsyncWait<'_> {
    fn drop(&mut self) {
        // make sure waker is cleared
        *self.sema.waker.lock().unwrap() = None;
    }
}

impl AsyncSema for DefaultSema {
    // unfortunately we can't define types with lifetimes
    // in traits, the best we can do is unsafely strip the
    // lifetime and leave it up to the caller to drop the
    // types in the correct order
    type AsyncWait = DefaultSemaAsyncWait<'static>;

    fn wait_async(&self, delta: Option<Delta>) -> Self::AsyncWait {
        // only allow one async wait at a time
        debug_assert!(self.waker.lock().unwrap().is_none());

        let wait = DefaultSemaAsyncWait {
            sema: self,
            timer: delta.map(|delta| Timer::after(Duration::from_millis(delta.ticks() as u64))),
        };

        // strip lifetime
        unsafe { transmute::<DefaultSemaAsyncWait<'_>, _>(wait) }
    }
}

