
// TODO feature gate this?
use core::sync::atomic::AtomicBool;
use core::sync::atomic::fence;
use core::sync::atomic;
use core::marker::PhantomData;
use core::mem::size_of;
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
use cfg_if::cfg_if;

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
cfg_if! {
    if #[cfg(feature="utick-at-least-u128")] {
        #[allow(non_camel_case_types)] pub type utick = u128;
        #[allow(non_camel_case_types)] pub type itick = i128;
        pub type NonZeroUtick = core::num::NonZeroI128;
        pub type NonZeroItick = core::num::NonZeroI128;

    } else if #[cfg(feature="utick-at-least-u64")] {
        #[allow(non_camel_case_types)] pub type utick = u64;
        #[allow(non_camel_case_types)] pub type itick = i64;
        pub type NonZeroUtick = core::num::NonZeroI64;
        pub type NonZeroItick = core::num::NonZeroI64;

    } else if #[cfg(feature="utick-at-least-u32")] {
        #[allow(non_camel_case_types)] pub type utick = u32;
        #[allow(non_camel_case_types)] pub type itick = i32;
        pub type NonZeroUtick = core::num::NonZeroI32;
        pub type NonZeroItick = core::num::NonZeroI32;

    } else if #[cfg(feature="utick-at-least-u16")] {
        #[allow(non_camel_case_types)] pub type utick = u16;
        #[allow(non_camel_case_types)] pub type itick = i16;
        pub type NonZeroUtick = core::num::NonZeroI16;
        pub type NonZeroItick = core::num::NonZeroI16;

    } else if #[cfg(feature="utick-at-least-u8")] {
        #[allow(non_camel_case_types)] pub type utick = u8;
        #[allow(non_camel_case_types)] pub type itick = i8;
        pub type NonZeroUtick = core::num::NonZeroI8;
        pub type NonZeroItick = core::num::NonZeroI8;
    }
}


// Atomic primitives

pub(crate) use core::sync::atomic::Ordering;

// The atomic double-word unit used in equeue
cfg_if! {
    if #[cfg(feature="udeptr-at-least-u128")] {
        #[allow(non_camel_case_types)] pub(crate) type udeptr = u128;
        #[allow(non_camel_case_types)] pub(crate) type ideptr = i128;
        pub(crate) type AtomicUdeptr = core::sync::atomic::AtomicU128;

    } else if #[cfg(feature="udeptr-at-least-u64")] {
        #[allow(non_camel_case_types)] pub(crate) type udeptr = u64;
        #[allow(non_camel_case_types)] pub(crate) type ideptr = i64;
        pub(crate) type AtomicUdeptr = core::sync::atomic::AtomicU64;

    } else if #[cfg(feature="udeptr-at-least-u32")] {
        #[allow(non_camel_case_types)] pub(crate) type udeptr = u32;
        #[allow(non_camel_case_types)] pub(crate) type ideptr = i32;
        pub(crate) type AtomicUdeptr = core::sync::atomic::AtomicU32;
    }
}

// Integer that fits an in-slab equeue pointer, should be 1/2 of a udeptr
cfg_if! {
    if #[cfg(feature="udeptr-at-least-u128")] {
        #[allow(non_camel_case_types)] pub(crate) type ueptr = u64;
        #[allow(non_camel_case_types)] pub(crate) type ieptr = i64;
        pub(crate) type AtomicUeptr = core::sync::atomic::AtomicU64;

    } else if #[cfg(feature="udeptr-at-least-u64")] {
        #[allow(non_camel_case_types)] pub(crate) type ueptr = u32;
        #[allow(non_camel_case_types)] pub(crate) type ieptr = i32;
        pub(crate) type AtomicUeptr = core::sync::atomic::AtomicU32;

    } else if #[cfg(feature="udeptr-at-least-u32")] {
        #[allow(non_camel_case_types)] pub(crate) type ueptr = u16;
        #[allow(non_camel_case_types)] pub(crate) type ieptr = i16;
        pub(crate) type AtomicUeptr = core::sync::atomic::AtomicU16;
    }
}

// Integer that fits a pointer generation count, should be 1/4 of a udeptr
cfg_if! {
    if #[cfg(feature="udeptr-at-least-u128")] {
        #[allow(non_camel_case_types)] pub(crate) type ugen = u32;
        #[allow(non_camel_case_types)] pub(crate) type igen = i32;

    } else if #[cfg(feature="udeptr-at-least-u64")] {
        #[allow(non_camel_case_types)] pub(crate) type ugen = u16;
        #[allow(non_camel_case_types)] pub(crate) type igen = i16;

    } else if #[cfg(feature="udeptr-at-least-u32")] {
        #[allow(non_camel_case_types)] pub(crate) type ugen = u8;
        #[allow(non_camel_case_types)] pub(crate) type igen = i8;
    }
}

// Locking primitive
#[derive(Debug)]
pub(crate) struct SysLock(Mutex<()>);

impl SysLock {
    pub(crate) fn new() -> Self {
        SysLock(Mutex::new(()))
    }
}

impl Lock for SysLock {
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


// Time/semaphore primitive
#[derive(Debug)]
pub struct SysClock {
    instant: Instant,

    mutex: Mutex<()>,
    cond: Condvar,
    waker: Mutex<Option<Waker>>,
}

impl SysClock {
    pub fn new() -> Self {
        Self {
            instant: Instant::now(),

            mutex: Mutex::new(()),
            cond: Condvar::new(),
            waker: Mutex::new(None),
        }
    }
}

impl Clock for SysClock {
    fn now(&self) -> utick {
        self.instant
            .elapsed()
            .as_millis()
            as utick
    }
}

impl TryIntoDelta<SysClock> for Duration {
    type Error = ();
    fn try_into_delta(self, _: &SysClock) -> Result<Delta, Self::Error> {
        itick::try_from(self.as_millis()).ok()
            .and_then(|ticks| Delta::new(ticks))
            .ok_or(())
    }
}

impl FromDelta<SysClock> for Duration {
    fn from_delta(_: &SysClock, delta: Delta) -> Self {
        Duration::from_millis(delta.ticks() as u64)
    }
}

impl Sema for SysClock {
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
pub struct SysClockAsyncWait<'a> {
    sema: &'a SysClock,
    timer: Option<Timer>,
}

impl Future for SysClockAsyncWait<'_> {
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

impl Drop for SysClockAsyncWait<'_> {
    fn drop(&mut self) {
        // make sure waker is cleared
        *self.sema.waker.lock().unwrap() = None;
    }
}

impl AsyncSema for SysClock {
    // unfortunately we can't define types with lifetimes
    // in traits, the best we can do is unsafely strip the
    // lifetime and leave it up to the caller to drop the
    // types in the correct order
    type AsyncWait = SysClockAsyncWait<'static>;

    fn wait_async(&self, delta: Option<Delta>) -> Self::AsyncWait {
        // only allow one async wait at a time
        debug_assert!(self.waker.lock().unwrap().is_none());

        let wait = SysClockAsyncWait {
            sema: self,
            timer: delta.map(|delta| Timer::after(Duration::from_millis(delta.ticks() as u64))),
        };

        // strip lifetime
        unsafe { transmute::<SysClockAsyncWait<'_>, _>(wait) }
    }
}

