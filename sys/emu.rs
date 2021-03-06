
// sys/emu.rs provides a similar integration with std/core as sys/std.rs, but
// with a fixed time that can only be moved by an explicit call to
// `SysClock::set_now`.
//
// This is useful for testing, or running in systems where time is not available
//

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
use core::fmt;

use cfg_if::cfg_if;

#[cfg(feature="alloc")] extern crate alloc as core_alloc;

#[cfg(feature="std")] use std::time::Instant;
#[cfg(feature="std")] use std::sync::Mutex;
#[cfg(feature="std")] use std::sync::MutexGuard;

use crate::traits::*;
use crate::util::*;
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
        pub type NonZeroUtick = core::num::NonZeroU128;
        pub type NonZeroItick = core::num::NonZeroI128;
        type AtomicUtick = core::sync::atomic::AtomicU128;

    } else if #[cfg(equeue_utick_width="64")] {
        #[allow(non_camel_case_types)] pub type utick = u64;
        #[allow(non_camel_case_types)] pub type itick = i64;
        pub type NonZeroUtick = core::num::NonZeroU64;
        pub type NonZeroItick = core::num::NonZeroI64;
        type AtomicUtick = core::sync::atomic::AtomicU64;

    } else if #[cfg(equeue_utick_width="32")] {
        #[allow(non_camel_case_types)] pub type utick = u32;
        #[allow(non_camel_case_types)] pub type itick = i32;
        pub type NonZeroUtick = core::num::NonZeroU32;
        pub type NonZeroItick = core::num::NonZeroI32;
        type AtomicUtick = core::sync::atomic::AtomicU32;

    } else if #[cfg(equeue_utick_width="16")] {
        #[allow(non_camel_case_types)] pub type utick = u16;
        #[allow(non_camel_case_types)] pub type itick = i16;
        pub type NonZeroUtick = core::num::NonZeroU16;
        pub type NonZeroItick = core::num::NonZeroI16;
        type AtomicUtick = core::sync::atomic::AtomicU16;

    } else if #[cfg(equeue_utick_width="8")] {
        #[allow(non_camel_case_types)] pub type utick = u8;
        #[allow(non_camel_case_types)] pub type itick = i8;
        pub type NonZeroUtick = core::num::NonZeroU8;
        pub type NonZeroItick = core::num::NonZeroI8;
        type AtomicUtick = core::sync::atomic::AtomicU8;
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
#[cfg(any(
    equeue_queue_mode="locking",
    equeue_alloc_mode="locking",
    equeue_break_mode="locking",
))]
cfg_if! {
    if #[cfg(all(feature="std", feature="measure-locks"))] {
        use core::sync::atomic::AtomicBool;
        static EQUEUE_LOCKED: AtomicBool = AtomicBool::new(false);

        // this is public just to get access to SysLock::is_locked
        #[derive(Debug)]
        pub struct SysLock(Mutex<()>);

        #[derive(Debug)]
        pub struct SysLockGuard<'a>(MutexGuard<'a, ()>);

        impl SysLock {
            pub(crate) fn new() -> Self {
                SysLock(Mutex::new(()))
            }

            pub fn is_locked() -> bool {
                EQUEUE_LOCKED.load(Ordering::SeqCst)
            }
        }

        impl Lock for SysLock {
            // unfortunately we can't define types with lifetimes
            // in traits, the best we can do is unsafely strip the
            // lifetime and leave it up to the caller to drop the
            // types in the correct order
            type Guard = SysLockGuard<'static>;

            fn lock(&self) -> Self::Guard {
                let guard = SysLockGuard(self.0.lock().unwrap());
                // note that we actually can't contend here, we have the lock!
                EQUEUE_LOCKED.store(true, Ordering::SeqCst);
                // strip lifetime
                unsafe { transmute::<SysLockGuard<'_>, _>(guard) }
            }
        }

        impl Drop for SysLockGuard<'_> {
            fn drop(&mut self) {
                // note that we actually can't contend here, we have the lock!
                EQUEUE_LOCKED.store(false, Ordering::SeqCst);
            }
        }
    } else if #[cfg(feature="std")] {
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
// TODO should this satisfy Sema and AsyncSema eventually?
#[derive(Debug)]
pub struct SysClock();

static EQUEUE_TICK: AtomicUtick = AtomicUtick::new(0);

impl SysClock {
    pub fn new() -> Self {
        Self()
    }

    // In order for loom to work, we need to constrain our clock to be
    // deterministic, instead of actually tracking time we open this up
    // to let the tests set the time artificially.
    //
    // Since internally we use a loom atomic, time updates should participate
    // in loom's reordering.
    pub fn set_now(now: utick) {
        EQUEUE_TICK.store(now, Ordering::SeqCst);
    }
}

impl Clock for SysClock {
    fn now(&self) -> utick {
        EQUEUE_TICK.load(Ordering::SeqCst)
    }
}

impl Signal for SysClock {
    fn signal(&self) {
        // do nothing, since we don't actually satisfy sema
    }
}
