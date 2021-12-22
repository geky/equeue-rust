
// TODO feature gate this?
use core::sync::atomic::AtomicUsize;
use core::sync::atomic::Ordering;



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
