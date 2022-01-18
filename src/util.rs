
// TODO feature gate this?
use core::sync::atomic::AtomicUsize;
use core::sync::atomic;
use core::marker::PhantomData;
use core::mem::size_of;
use core::mem::transmute;
use core::cmp::Ordering;


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

impl Scmp for u128 {
    type Output = i128;

    #[inline]
    fn sdiff(self, b: u128) -> i128 {
        self.wrapping_sub(b) as i128
    }

    #[inline]
    fn scmp(self, b: u128) -> Ordering {
        self.sdiff(b).cmp(&0)
    }
}

impl Scmp for u64 {
    type Output = i64;

    #[inline]
    fn sdiff(self, b: u64) -> i64 {
        self.wrapping_sub(b) as i64
    }

    #[inline]
    fn scmp(self, b: u64) -> Ordering {
        self.sdiff(b).cmp(&0)
    }
}

impl Scmp for u32 {
    type Output = i32;

    #[inline]
    fn sdiff(self, b: u32) -> i32 {
        self.wrapping_sub(b) as i32
    }

    #[inline]
    fn scmp(self, b: u32) -> Ordering {
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

