
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
use core::convert::Infallible;
use core::ops::Deref;

use cfg_if::cfg_if;

#[cfg(feature="alloc")] extern crate alloc as core_alloc;

#[cfg(feature="std")] use std::time::Instant;
#[cfg(feature="std")] use std::sync::Mutex;
#[cfg(feature="std")] use std::sync::MutexGuard;
#[cfg(feature="std")] use std::sync::Condvar;
#[cfg(feature="std")] use std::collections::HashMap;

#[cfg(feature="async-io")] use async_io::Timer;
#[cfg(feature="async-std")] use async_std::task::sleep;
#[cfg(feature="tokio")] use tokio::time::{sleep, Sleep};

use crate::traits::*;
use crate::Error;
use crate::Delta;


// Memory allocation, this is optional
#[cfg(feature="alloc")] pub(crate) use core_alloc::alloc::alloc;
#[cfg(feature="alloc")] pub(crate) use core_alloc::alloc::dealloc;


// Time primitives
cfg_if! {
    if #[cfg(equeue_utick_width="128")] {
        #[allow(non_camel_case_types)] pub type utick = u128;
        #[allow(non_camel_case_types)] pub type itick = i128;
        pub type NonZeroUtick = core::num::NonZeroI128;
        pub type NonZeroItick = core::num::NonZeroI128;

    } else if #[cfg(equeue_utick_width="64")] {
        #[allow(non_camel_case_types)] pub type utick = u64;
        #[allow(non_camel_case_types)] pub type itick = i64;
        pub type NonZeroUtick = core::num::NonZeroI64;
        pub type NonZeroItick = core::num::NonZeroI64;

    } else if #[cfg(equeue_utick_width="32")] {
        #[allow(non_camel_case_types)] pub type utick = u32;
        #[allow(non_camel_case_types)] pub type itick = i32;
        pub type NonZeroUtick = core::num::NonZeroI32;
        pub type NonZeroItick = core::num::NonZeroI32;

    } else if #[cfg(equeue_utick_width="16")] {
        #[allow(non_camel_case_types)] pub type utick = u16;
        #[allow(non_camel_case_types)] pub type itick = i16;
        pub type NonZeroUtick = core::num::NonZeroI16;
        pub type NonZeroItick = core::num::NonZeroI16;

    } else if #[cfg(equeue_utick_width="8")] {
        #[allow(non_camel_case_types)] pub type utick = u8;
        #[allow(non_camel_case_types)] pub type itick = i8;
        pub type NonZeroUtick = core::num::NonZeroI8;
        pub type NonZeroItick = core::num::NonZeroI8;
    }
}


// Atomic primitives

pub(crate) use core::sync::atomic::Ordering;

cfg_if! {
    if #[cfg(any(
        equeue_udeptr_width="128",
        all(equeue_udeptr_width="native", target_pointer_width="128")
    ))] {
        // The atomic double-eptr unit used in equeue
        #[allow(non_camel_case_types)] pub(crate) type udeptr = u128;
        #[allow(non_camel_case_types)] pub(crate) type ideptr = i128;
        pub(crate) type AtomicUdeptr = core::sync::atomic::AtomicU128;

        // Integer that fits an in-slab equeue pointer, should be 1/2 of a udeptr
        #[allow(non_camel_case_types)] pub(crate) type ueptr = u64;
        #[allow(non_camel_case_types)] pub(crate) type ieptr = i64;
        pub(crate) type AtomicUeptr = core::sync::atomic::AtomicU64;

        // Integer that fits a pointer generation count, should be 1/4 of a udeptr
        #[allow(non_camel_case_types)] pub(crate) type ugen = u32;
        #[allow(non_camel_case_types)] pub(crate) type igen = i32;

    } else if #[cfg(any(
        equeue_udeptr_width="64",
        all(equeue_udeptr_width="native", target_pointer_width="64")
    ))] {
        // The atomic double-eptr unit used in equeue
        #[allow(non_camel_case_types)] pub(crate) type udeptr = u64;
        #[allow(non_camel_case_types)] pub(crate) type ideptr = i64;
        pub(crate) type AtomicUdeptr = core::sync::atomic::AtomicU64;

        // Integer that fits an in-slab equeue pointer, should be 1/2 of a udeptr
        #[allow(non_camel_case_types)] pub(crate) type ueptr = u32;
        #[allow(non_camel_case_types)] pub(crate) type ieptr = i32;
        pub(crate) type AtomicUeptr = core::sync::atomic::AtomicU32;

        // Integer that fits a pointer generation count, should be 1/4 of a udeptr
        #[allow(non_camel_case_types)] pub(crate) type ugen = u16;
        #[allow(non_camel_case_types)] pub(crate) type igen = i16;

    } else if #[cfg(any(
        equeue_udeptr_width="32",
        all(equeue_udeptr_width="native", target_pointer_width="32")
    ))] {
        // The atomic double-eptr unit used in equeue
        #[allow(non_camel_case_types)] pub(crate) type udeptr = u32;
        #[allow(non_camel_case_types)] pub(crate) type ideptr = i32;
        pub(crate) type AtomicUdeptr = core::sync::atomic::AtomicU32;

        // Integer that fits an in-slab equeue pointer, should be 1/2 of a udeptr
        #[allow(non_camel_case_types)] pub(crate) type ueptr = u16;
        #[allow(non_camel_case_types)] pub(crate) type ieptr = i16;
        pub(crate) type AtomicUeptr = core::sync::atomic::AtomicU16;

        // Integer that fits a pointer generation count, should be 1/4 of a udeptr
        #[allow(non_camel_case_types)] pub(crate) type ugen = u8;
        #[allow(non_camel_case_types)] pub(crate) type igen = i8;
    }
}


// Locking primitive
cfg_if! {
    if #[cfg(feature="std")] {
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
    } else {
        // spinlock implementation, which is _terrible_ for multi-core,
        // but at least compiles in no-std and is acceptable for single-thread
        // use cases (ie no contention, and really no need for synchronization)
        #[derive(Debug)]
        pub(crate) struct SysLock(AtomicUeptr);

        impl SysLock {
            pub(crate) fn new() -> Self {
                SysLock(AtomicUeptr::new(0))
            }
        }

        pub(crate) struct SysLockGuard<'a>(&'a SysLock);

        impl Drop for SysLockGuard<'_> {
            fn drop(&mut self) {
                self.0.0.store(0, Ordering::Release);
            }
        }

        impl Lock for SysLock {
            // unfortunately we can't define types with lifetimes
            // in traits, the best we can do is unsafely strip the
            // lifetime and leave it up to the caller to drop the
            // types in the correct order
            type Guard = SysLockGuard<'static>;

            fn lock(&self) -> Self::Guard {
                // try to acquire lock
                loop {
                    if let Err(_) = self.0.compare_exchange(
                        0, 1,
                        Ordering::Acquire, Ordering::Relaxed
                    ) {
                        continue;
                    }

                    break;
                }

                // strip lifetime
                let guard = SysLockGuard(&self);
                unsafe { transmute::<SysLockGuard<'_>, _>(guard) }
            }
        }
    }
}


// Time/semaphore primitive
cfg_if! {
    if #[cfg(feature="std")] {
        #[derive(Debug)]
        pub struct SysClock {
            instant: Instant,

            flag: Mutex<i32>,
            cond: Condvar,

            #[cfg(any(feature="async-io", feature="async-std", feature="tokio"))]
            wakers: Mutex<HashMap<usize, Waker>>,
        }

        impl SysClock {
            pub fn new() -> Self {
                Self {
                    instant: Instant::now(),

                    flag: Mutex::new(0),
                    cond: Condvar::new(),

                    #[cfg(any(feature="async-io", feature="async-std", feature="tokio"))]
                    wakers: Mutex::new(HashMap::new()),
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

        impl TryFromDelta<SysClock> for Duration {
            type Error = Infallible;
            fn try_from_delta(_: &SysClock, delta: Delta) -> Result<Self, Self::Error> {
                Ok(Duration::from_millis(delta.ticks() as u64))
            }
        }

        cfg_if! {
            if #[cfg(feature="embedded-time")] {
                impl<T: embedded_time::TimeInt> TryIntoDelta<SysClock> for embedded_time::duration::Generic<T>
                where
                    utick: TryFrom<T>
                {
                    type Error = ();
                    fn try_into_delta(self, _: &SysClock) -> Result<Delta, Self::Error> {
                        embedded_time::duration::Milliseconds::<utick>::try_from(self).ok()
                            .and_then(|ticks| itick::try_from(ticks.0).ok())
                            .and_then(|ticks| Delta::new(ticks))
                            .ok_or(())
                    }
                }

                impl<T: embedded_time::TimeInt> TryFromDelta<SysClock> for embedded_time::duration::Generic<T>
                where
                    T: TryFrom<utick>
                {
                    type Error = ();
                    fn try_from_delta(_: &SysClock, delta: Delta) -> Result<Self, Self::Error> {
                        use embedded_time::duration::Duration;
                        embedded_time::duration::Milliseconds(delta.ticks() as utick)
                            .to_generic(embedded_time::fraction::Fraction::new(1, 1000)).ok()
                            .ok_or(())
                    }
                }

                impl<T: embedded_time::TimeInt> TryIntoDelta<SysClock> for embedded_time::duration::Nanoseconds<T>
                where
                    utick: TryFrom<T>
                {
                    type Error = ();
                    fn try_into_delta(self, clock: &SysClock) -> Result<Delta, Self::Error> {
                        embedded_time::duration::Generic::from(self).try_into_delta(clock)
                    }
                }

                impl<T: embedded_time::TimeInt> TryFromDelta<SysClock> for embedded_time::duration::Nanoseconds<T>
                where
                    T: TryFrom<utick>
                {
                    type Error = ();
                    fn try_from_delta(clock: &SysClock, delta: Delta) -> Result<Self, Self::Error> {
                        embedded_time::duration::Generic::<utick>::try_from_delta(clock, delta).ok()
                            .and_then(|ticks| Self::try_from(ticks).ok())
                            .ok_or(())
                    }
                }

                impl<T: embedded_time::TimeInt> TryIntoDelta<SysClock> for embedded_time::duration::Microseconds<T>
                where
                    utick: TryFrom<T>
                {
                    type Error = ();
                    fn try_into_delta(self, clock: &SysClock) -> Result<Delta, Self::Error> {
                        embedded_time::duration::Generic::from(self).try_into_delta(clock)
                    }
                }

                impl<T: embedded_time::TimeInt> TryFromDelta<SysClock> for embedded_time::duration::Microseconds<T>
                where
                    T: TryFrom<utick>
                {
                    type Error = ();
                    fn try_from_delta(clock: &SysClock, delta: Delta) -> Result<Self, Self::Error> {
                        embedded_time::duration::Generic::<utick>::try_from_delta(clock, delta).ok()
                            .and_then(|ticks| Self::try_from(ticks).ok())
                            .ok_or(())
                    }
                }

                impl<T: embedded_time::TimeInt> TryIntoDelta<SysClock> for embedded_time::duration::Milliseconds<T>
                where
                    utick: TryFrom<T>
                {
                    type Error = ();
                    fn try_into_delta(self, clock: &SysClock) -> Result<Delta, Self::Error> {
                        embedded_time::duration::Generic::from(self).try_into_delta(clock)
                    }
                }

                impl<T: embedded_time::TimeInt> TryFromDelta<SysClock> for embedded_time::duration::Milliseconds<T>
                where
                    T: TryFrom<utick>
                {
                    type Error = ();
                    fn try_from_delta(clock: &SysClock, delta: Delta) -> Result<Self, Self::Error> {
                        embedded_time::duration::Generic::<utick>::try_from_delta(clock, delta).ok()
                            .and_then(|ticks| Self::try_from(ticks).ok())
                            .ok_or(())
                    }
                }

                impl<T: embedded_time::TimeInt> TryIntoDelta<SysClock> for embedded_time::duration::Seconds<T>
                where
                    utick: TryFrom<T>
                {
                    type Error = ();
                    fn try_into_delta(self, clock: &SysClock) -> Result<Delta, Self::Error> {
                        embedded_time::duration::Generic::from(self).try_into_delta(clock)
                    }
                }

                impl<T: embedded_time::TimeInt> TryFromDelta<SysClock> for embedded_time::duration::Seconds<T>
                where
                    T: TryFrom<utick>
                {
                    type Error = ();
                    fn try_from_delta(clock: &SysClock, delta: Delta) -> Result<Self, Self::Error> {
                        embedded_time::duration::Generic::<utick>::try_from_delta(clock, delta).ok()
                            .and_then(|ticks| Self::try_from(ticks).ok())
                            .ok_or(())
                    }
                }

                impl<T: embedded_time::TimeInt> TryIntoDelta<SysClock> for embedded_time::duration::Minutes<T>
                where
                    utick: TryFrom<T>
                {
                    type Error = ();
                    fn try_into_delta(self, clock: &SysClock) -> Result<Delta, Self::Error> {
                        embedded_time::duration::Generic::from(self).try_into_delta(clock)
                    }
                }

                impl<T: embedded_time::TimeInt> TryFromDelta<SysClock> for embedded_time::duration::Minutes<T>
                where
                    T: TryFrom<utick>
                {
                    type Error = ();
                    fn try_from_delta(clock: &SysClock, delta: Delta) -> Result<Self, Self::Error> {
                        embedded_time::duration::Generic::<utick>::try_from_delta(clock, delta).ok()
                            .and_then(|ticks| Self::try_from(ticks).ok())
                            .ok_or(())
                    }
                }

                impl<T: embedded_time::TimeInt> TryIntoDelta<SysClock> for embedded_time::duration::Hours<T>
                where
                    utick: TryFrom<T>
                {
                    type Error = ();
                    fn try_into_delta(self, clock: &SysClock) -> Result<Delta, Self::Error> {
                        embedded_time::duration::Generic::from(self).try_into_delta(clock)
                    }
                }

                impl<T: embedded_time::TimeInt> TryFromDelta<SysClock> for embedded_time::duration::Hours<T>
                where
                    T: TryFrom<utick>
                {
                    type Error = ();
                    fn try_from_delta(clock: &SysClock, delta: Delta) -> Result<Self, Self::Error> {
                        embedded_time::duration::Generic::<utick>::try_from_delta(clock, delta).ok()
                            .and_then(|ticks| Self::try_from(ticks).ok())
                            .ok_or(())
                    }
                }
            }
        }

        impl Sema for SysClock {
            fn signal(&self) {
                let mut flag = self.flag.lock().unwrap();
                *flag = -*flag + 1;
                drop(flag);

                self.cond.notify_all();

                #[cfg(any(feature="async-io", feature="async-std", feature="tokio"))]
                {
                    for (_, waker) in self.wakers.lock().unwrap().drain() {
                        waker.wake();
                    }
                }
            }

            fn wait(&self, delta: Option<Delta>) {
                // already signaled?
                let mut flag = self.flag.lock().unwrap();
                if *flag > 0 {
                    *flag -= 1;
                    return;
                }

                // otherwise we still decrement to indicate how many threads are
                // waiting, this may lead to spurious wakeups but avoids not waking
                // up all threads
                *flag -= 1;

                if let Some(delta) = delta {
                    let _ = self.cond
                        .wait_timeout(flag, Duration::from_millis(delta.ticks() as u64))
                        .unwrap();
                } else {
                    let _ = self.cond
                        .wait(flag)
                        .unwrap();
                }
            }
        }

        #[cfg(any(feature="async-io", feature="async-std", feature="tokio"))]
        #[derive(Debug)]
        pub struct SysClockAsyncWait<'a, T> {
            sema: &'a SysClock,
            timer: Option<T>,
        }

        #[cfg(any(feature="async-io", feature="async-std", feature="tokio"))]
        impl<T: Future<Output=R> + Send, R> Future for SysClockAsyncWait<'_, T> {
            type Output = ();

            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                // already signaled?
                let mut flag = self.sema.flag.lock().unwrap();
                if *flag > 0 {
                    *flag -= 1;
                    return Poll::Ready(());
                }

                // otherwise we still decrement to indicate how many threads are
                // waiting, this may lead to spurious wakeups but avoids not waking
                // up all threads
                *flag -= 1;

                // save waker for signaling
                self.sema.wakers.lock().unwrap().insert(
                    self.deref() as *const _ as usize, 
                    cx.waker().clone()
                );

                if let Some(ref mut timer) = unsafe { &mut self.get_unchecked_mut().timer } {
                    // wait on timer
                    unsafe { Pin::new_unchecked(timer) }
                        .poll(cx)
                        .map(|_| ())
                } else {
                    Poll::Pending
                }
            }
        }

        #[cfg(any(feature="async-io", feature="async-std", feature="tokio"))]
        impl<T> Drop for SysClockAsyncWait<'_, T> {
            fn drop(&mut self) {
                // make sure our waker is cleared
                self.sema.wakers.lock().unwrap()
                    .remove(&(self as *const _ as usize));
            }
        }

        cfg_if! {
            if #[cfg(feature="async-io")] {
                impl AsyncSema for SysClock {
                    // unfortunately we can't define types with lifetimes
                    // in traits, the best we can do is unsafely strip the
                    // lifetime and leave it up to the caller to drop the
                    // types in the correct order
                    type AsyncWait = SysClockAsyncWait<'static, Timer>;

                    fn wait_async(&self, delta: Option<Delta>) -> Self::AsyncWait {
                        let wait = SysClockAsyncWait {
                            sema: self,
                            timer: delta.map(|delta|
                                Timer::after(Duration::from_millis(delta.ticks() as u64))
                            ),
                        };

                        // strip lifetime
                        unsafe { transmute::<SysClockAsyncWait<'_, Timer>, _>(wait) }
                    }
                }
            } else if #[cfg(feature="async-std")] {
                impl AsyncSema for SysClock {
                    // unfortunately we can't define types with lifetimes
                    // in traits, the best we can do is unsafely strip the
                    // lifetime and leave it up to the caller to drop the
                    // types in the correct order
                    type AsyncWait = SysClockAsyncWait<'static, Pin<Box<dyn Future<Output=()> + Send>>>;

                    fn wait_async(&self, delta: Option<Delta>) -> Self::AsyncWait {
                        let wait = SysClockAsyncWait {
                            sema: self,
                            timer: delta.map(|delta|
                                Box::pin(sleep(Duration::from_millis(delta.ticks() as u64)))
                                    as Pin<Box<dyn Future<Output=()> + Send>>
                            ),
                        };

                        // strip lifetime
                        unsafe { transmute::<SysClockAsyncWait<'_, Pin<Box<dyn Future<Output=()> + Send>>>, _>(wait) }
                    }
                }
            } else if #[cfg(feature="tokio")] {
                impl AsyncSema for SysClock {
                    // unfortunately we can't define types with lifetimes
                    // in traits, the best we can do is unsafely strip the
                    // lifetime and leave it up to the caller to drop the
                    // types in the correct order
                    type AsyncWait = SysClockAsyncWait<'static, Sleep>;

                    fn wait_async(&self, delta: Option<Delta>) -> Self::AsyncWait {
                        let wait = SysClockAsyncWait {
                            sema: self,
                            timer: delta.map(|delta|
                                sleep(Duration::from_millis(delta.ticks() as u64))
                            ),
                        };

                        // strip lifetime
                        unsafe { transmute::<SysClockAsyncWait<'_, Sleep>, _>(wait) }
                    }
                }
            }
        }
    } else {
        #[derive(Debug)]
        pub struct SysClock();
    }
}
