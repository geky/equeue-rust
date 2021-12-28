
// TODO feature gate this?
use core::sync::atomic::AtomicUsize;
use core::sync::atomic;
use core::marker::PhantomData;
use core::mem::size_of;
use core::cmp::Ordering;

use std::time::Instant;
use std::time::Duration;
use std::sync::Mutex;
use std::sync::Condvar;


// min/max
pub(crate) use core::cmp::min;
pub(crate) use core::cmp::max;

// alignup/aligndown
pub(crate) trait Align {
    fn alignup(self, align: usize) -> Self;
    fn aligndown(self, align: usize) -> Self;
}

impl Align for usize {
    #[inline]
    fn aligndown(self, align: usize) -> usize {
        self - (self % align)
    }

    #[inline]
    fn alignup(self, align: usize) -> usize {
        (self + align-1).aligndown(align)
    }
}

impl Align for *const u8 {
    #[inline]
    fn aligndown(self, align: usize) -> *const u8 {
        (self as usize).aligndown(align) as *const u8
    }

    #[inline]
    fn alignup(self, align: usize) -> *const u8 {
        (self as usize).alignup(align) as *const u8
    }
}

impl Align for *mut u8 {
    #[inline]
    fn aligndown(self, align: usize) -> *mut u8 {
        (self as usize).aligndown(align) as *mut u8
    }

    #[inline]
    fn alignup(self, align: usize) -> *mut u8 {
        (self as usize).alignup(align) as *mut u8
    }
}

#[inline]
pub(crate) fn aligndown<T: Align>(a: T, align: usize) -> T {
    a.aligndown(align)
}

#[inline]
pub(crate) fn alignup<T: Align>(a: T, align: usize) -> T {
    a.alignup(align)
}

// npw2
pub(crate) trait Npw2 {
    fn npw2(self) -> u8;
}

impl Npw2 for usize {
    #[inline]
    fn npw2(self) -> u8 {
        self.next_power_of_two().trailing_zeros() as u8
    }
}

#[inline]
pub(crate) fn npw2<T: Npw2>(a: T) -> u8 {
    a.npw2()
}

// scmp/sdiff
pub(crate) trait Scmp {
    type Output;
    fn sdiff(self, b: Self) -> Self::Output;
    fn scmp(self, b: Self) -> Ordering;
}

impl Scmp for utick {
    type Output = itick;

    #[inline]
    fn sdiff(self, b: utick) -> itick {
        self.wrapping_sub(b) as itick
    }

    #[inline]
    fn scmp(self, b: utick) -> Ordering {
        self.sdiff(b).cmp(&0)
    }
}

impl Scmp for usize {
    type Output = isize;

    #[inline]
    fn sdiff(self, b: usize) -> isize {
        self.wrapping_sub(b) as isize
    }

    #[inline]
    fn scmp(self, b: usize) -> Ordering {
        self.sdiff(b).cmp(&0)
    }
}

#[inline]
pub(crate) fn sdiff<T: Scmp>(a: T, b: T) -> <T as Scmp>::Output {
    a.sdiff(b)
}

#[inline]
pub(crate) fn scmp<T: Scmp>(a: T, b: T) -> Ordering {
    a.scmp(b)
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
        unsafe { *(&self.0.load(atomic::Ordering::SeqCst) as *const _ as *const T) }
    }

    /// Atomic compare-and-swap
    pub fn cas(&self, old: T, new: T) -> Result<T, T> where T: Copy {
        unsafe { 
            //*(&self.0.compare_exchange_weak(
            *(&self.0.compare_exchange(
                *(&old as *const _ as *const usize),
                *(&new as *const _ as *const usize),
                atomic::Ordering::SeqCst,
                atomic::Ordering::SeqCst
            ) as *const _ as *const Result<T, T>)
        }
    }

    /// Non-atomic load iff we have exclusive access, we can
    /// leverage Rust's mut for this
    pub fn load_ex(&mut self) -> T where T: Copy {
        unsafe { *(&self.0.load(atomic::Ordering::Relaxed) as *const _ as *const T) }
    }

    /// Non-atomic store iff we have exclusive access, we can
    /// leverage Rust's mut for this
    pub fn store_ex(&mut self, new: T) {
        self.0.store(
            unsafe { *(&new as *const _ as *const usize) },
            atomic::Ordering::Relaxed
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
