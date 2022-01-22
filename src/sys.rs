
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

use cfg_if::cfg_if;

#[cfg(feature="alloc")] extern crate alloc as core_alloc;

#[cfg(feature="std")] use std::time::Instant;
#[cfg(feature="std")] use std::sync::Mutex;
#[cfg(feature="std")] use std::sync::MutexGuard;
#[cfg(feature="std")] use std::sync::Condvar;

#[cfg(feature="async-io")] use async_io::Timer;

#[cfg(feature="async-std")] use async_std::channel;
#[cfg(feature="async-std")] use async_std::future::timeout;

#[cfg(feature="tokio")] use tokio::sync::watch;
#[cfg(feature="tokio")] use tokio::time::timeout;

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

// The atomic double-word unit used in equeue
cfg_if! {
    if #[cfg(equeue_udeptr_width="128")] {
        #[allow(non_camel_case_types)] pub(crate) type udeptr = u128;
        #[allow(non_camel_case_types)] pub(crate) type ideptr = i128;
        pub(crate) type AtomicUdeptr = core::sync::atomic::AtomicU128;

    } else if #[cfg(equeue_udeptr_width="64")] {
        #[allow(non_camel_case_types)] pub(crate) type udeptr = u64;
        #[allow(non_camel_case_types)] pub(crate) type ideptr = i64;
        pub(crate) type AtomicUdeptr = core::sync::atomic::AtomicU64;

    } else if #[cfg(equeue_udeptr_width="32")] {
        #[allow(non_camel_case_types)] pub(crate) type udeptr = u32;
        #[allow(non_camel_case_types)] pub(crate) type ideptr = i32;
        pub(crate) type AtomicUdeptr = core::sync::atomic::AtomicU32;
    }
}

// Integer that fits an in-slab equeue pointer, should be 1/2 of a udeptr
cfg_if! {
    if #[cfg(equeue_udeptr_width="128")] {
        #[allow(non_camel_case_types)] pub(crate) type ueptr = u64;
        #[allow(non_camel_case_types)] pub(crate) type ieptr = i64;
        pub(crate) type AtomicUeptr = core::sync::atomic::AtomicU64;

    } else if #[cfg(equeue_udeptr_width="64")] {
        #[allow(non_camel_case_types)] pub(crate) type ueptr = u32;
        #[allow(non_camel_case_types)] pub(crate) type ieptr = i32;
        pub(crate) type AtomicUeptr = core::sync::atomic::AtomicU32;

    } else if #[cfg(equeue_udeptr_width="32")] {
        #[allow(non_camel_case_types)] pub(crate) type ueptr = u16;
        #[allow(non_camel_case_types)] pub(crate) type ieptr = i16;
        pub(crate) type AtomicUeptr = core::sync::atomic::AtomicU16;
    }
}

// Integer that fits a pointer generation count, should be 1/4 of a udeptr
cfg_if! {
    if #[cfg(equeue_udeptr_width="128")] {
        #[allow(non_camel_case_types)] pub(crate) type ugen = u32;
        #[allow(non_camel_case_types)] pub(crate) type igen = i32;

    } else if #[cfg(equeue_udeptr_width="64")] {
        #[allow(non_camel_case_types)] pub(crate) type ugen = u16;
        #[allow(non_camel_case_types)] pub(crate) type igen = i16;

    } else if #[cfg(equeue_udeptr_width="32")] {
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

            mutex: Mutex<()>,
            cond: Condvar,

            #[cfg(feature="async-io")] waker: Mutex<Option<Waker>>,
            #[cfg(feature="async-std")] channel: (channel::Sender<()>, channel::Receiver<()>),
            #[cfg(feature="tokio")] watch: (watch::Sender<()>, watch::Receiver<()>),
        }

        impl SysClock {
            pub fn new() -> Self {
                Self {
                    instant: Instant::now(),

                    mutex: Mutex::new(()),
                    cond: Condvar::new(),

                    #[cfg(feature="async-io")] waker: Mutex::new(None),
                    #[cfg(feature="async-std")] channel: channel::bounded(1),
                    #[cfg(feature="tokio")] watch: watch::channel(()),
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
                self.cond.notify_all();

                #[cfg(feature="async-io")]
                {
                    let waker = self.waker.lock().unwrap().take();
                    if let Some(waker) = waker {
                        waker.wake();
                    }
                }

                #[cfg(feature="async-std")]
                let _ = self.channel.0.try_send(());

                #[cfg(feature="tokio")]
                self.watch.0.send(()).unwrap();
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

        cfg_if! {
            if #[cfg(feature="async-io")] {
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

            } else if #[cfg(feature="async-std")] {
                impl AsyncSema for SysClock {
                    // unfortunately we can't define types with lifetimes
                    // in traits, the best we can do is unsafely strip the
                    // lifetime and leave it up to the caller to drop the
                    // types in the correct order
                    type AsyncWait = Pin<Box<dyn Future<Output=()> + Send>>;

                    fn wait_async(&self, delta: Option<Delta>) -> Self::AsyncWait {
                        let self_ = unsafe { transmute::<_, &'static SysClock>(self) };
                        match delta {
                            Some(delta) => {
                                Box::pin(async move {
                                    let _ = timeout(
                                        Duration::from_millis(delta.ticks() as u64),
                                        self_.channel.1.recv()
                                    ).await;
                                })
                            }
                            None => {
                                Box::pin(async move {
                                    let _ = self_.channel.1.recv().await;
                                })
                            }
                        }
                    }
                }
            } else if #[cfg(feature="tokio")] {
                impl AsyncSema for SysClock {
                    // unfortunately we can't define types with lifetimes
                    // in traits, the best we can do is unsafely strip the
                    // lifetime and leave it up to the caller to drop the
                    // types in the correct order
                    type AsyncWait = Pin<Box<dyn Future<Output=()> + Send>>;

                    fn wait_async(&self, delta: Option<Delta>) -> Self::AsyncWait {
                        let self_ = unsafe { transmute::<_, &'static SysClock>(self) };
                        let mut receiver = self_.watch.1.clone();
                        match delta {
                            Some(delta) => {
                                Box::pin(async move {
                                    let _ = timeout(
                                        Duration::from_millis(delta.ticks() as u64),
                                        receiver.changed()
                                    ).await;
                                })
                            }
                            None => {
                                Box::pin(async move {
                                    let _ = receiver.changed().await;
                                })
                            }
                        }
                    }
                }
            }
        }
    } else {
        #[derive(Debug)]
        pub struct SysClock();
    }
}
