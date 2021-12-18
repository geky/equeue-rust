
// TODO feature gate this?
use core::sync::atomic::AtomicUsize;
pub(crate) use core::sync::atomic::Ordering;
pub(crate) use core::sync::atomic::fence;
pub(crate) use core::sync::atomic::compiler_fence;


pub(crate) use core::cmp::max;
pub(crate) use core::cmp::min;


#[inline]
pub(crate) fn aligndown(a: usize, align: usize) -> usize {
    a - (a % align)
}

#[inline]
pub(crate) fn alignup(a: usize, align: usize) -> usize {
    aligndown(a + align-1, align)
}

#[inline]
pub(crate) fn npw2(a: usize) -> u8 {
    a.next_power_of_two().trailing_zeros() as u8
}


#[inline]
pub(crate) unsafe fn aload(a: *const usize) -> usize {
    (&*(a as *const AtomicUsize)).load(Ordering::SeqCst)
}

#[inline]
pub(crate) unsafe fn acas(
    a: *const usize,
    old: usize,
    new: usize
) -> Result<usize, usize> {
    (&*(a as *const AtomicUsize)).compare_exchange_weak(
        old,
        new, 
        Ordering::SeqCst,
        Ordering::SeqCst,
    )
}
