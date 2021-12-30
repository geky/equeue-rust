
// TODO enable
//#![no_std]

#![deny(missing_debug_implementations)]

// TODO remove this eventually
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use core::mem::size_of;
use core::mem::align_of;
use core::fmt;
use core::alloc::Layout;
use core::ptr;
use core::slice;
use core::marker::PhantomData;
use core::ops::Deref;
use core::ops::DerefMut;
use core::borrow::Borrow;
use core::borrow::BorrowMut;
use core::mem::forget;
use core::ptr::drop_in_place;
use core::mem::transmute;
use core::cmp::Ordering;
use core::num::NonZeroUsize;
use core::num::TryFromIntError;
use core::fmt::Debug;

mod util;
mod sys;
use util::*;
use sys::*;


/// Event queue errors
#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    NoMem,
    Overflow,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::NoMem => write!(f, "Out of memory"),
            Error::Overflow => write!(f, "Value could not fit in type"),
        }
    }
}

/// Small wrapper to generalize atomics to any ueptr sized type
#[repr(transparent)]
struct Atomic<T>(AtomicUeptr, PhantomData<T>);

impl<T: Copy + Debug> Debug for Atomic<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Atomic")
            .field(&self.load())
            .finish()
    }
}

impl<T> Atomic<T> {
    fn new(t: T) -> Self {
        debug_assert_eq!(size_of::<T>(), size_of::<ueptr>());
        Self(
            AtomicUeptr::new(unsafe { *(&t as *const _ as *const usize) }),
            PhantomData
        )
    }

    fn load(&self) -> T where T: Copy {
        unsafe { *(&self.0.load() as *const _ as *const T) }
    }

    fn store(&self, v: T) {
        self.0.store(unsafe { *(&v as *const _ as *const usize) })
    }

//    fn load_ex(&mut self) -> T where T: Copy {
//        unsafe { *(&self.0.load_ex() as *const _ as *const T) }
//    }
//
//    fn store_ex(&mut self, v: T) {
//        self.0.store_ex(unsafe { *(&v as *const _ as *const usize) })
//    }
}

/// Slab-internal pointer, with internalized generation count
#[derive(Copy, Clone, Eq, PartialEq)]
#[repr(transparent)]
struct Eptr(ueptr);

impl Debug for Eptr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // these really need to be in hex to be readable
        f.debug_tuple("Eptr")
            .field(&format_args!("{:#x}", self.0))
            .finish()
    }
}

impl Eptr {
    // maximum alignment of internal allocations, this must be larger than
    // Eptr's alignment, and pointer alignment is a common alignment
    //
    // TODO should these take advantage of the npw2(event size) to be compressed
    // even further?
    //
    const ALIGN: usize = {
        let mut align = align_of::<Ebuf>();
        if align_of::<Eptr>() > align { align = align_of::<Eptr>() }
        if align_of::<*const usize>() > align { align = align_of::<*const usize>() }
        align
    };

    const fn null() -> Eptr {
        Eptr(0)
    }

    // this is unsafe because a generation mismatch risks access issues
    unsafe fn from_parts(q: &Equeue, gen: ueptr, e: &Ebuf) -> Eptr {
        let eptr = (e as *const Ebuf as *const u8)
            .offset_from(q.slab.as_ptr())
            as usize
            / Eptr::ALIGN;

        Eptr((gen << q.npw2) | eptr)
    }

    fn from(q: &Equeue, e: &Ebuf) -> Eptr {
        unsafe { Eptr::from_parts(q, e.gen(q), e) }
    }

    fn as_ref<'a>(&self, q: &'a Equeue) -> Option<&'a Ebuf> {
        let mask = (1 << q.npw2) - 1;
        match self.0 & mask {
            0 => None,
            _ => Some(
                unsafe {
                    &*(&q.slab[(self.0 & mask) * Eptr::ALIGN]
                        as *const u8 as *const Ebuf)
                }
            )
        }
    }

    fn gen(&self, q: &Equeue) -> ueptr {
        self.0 >> q.npw2
    }

    fn inc(&self, q: &Equeue) -> Eptr {
        Eptr(self.0.wrapping_add(1 << q.npw2))
    }
}

/// Some event bookkeeping info crammed into a ueptr
#[derive(Copy, Clone)]
#[repr(transparent)]
struct Einfo(ueptr);

impl Debug for Einfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // these really need to be in hex to be readable
        f.debug_tuple("Einfo")
            .field(&format_args!("{:#x}", self.0))
            .finish()
    }
}

impl Einfo {
    fn new(q: &Equeue, gen: ueptr, id: ueptr, npw2: u8) -> Einfo {
        debug_assert!(gen < (1 << 8*size_of::<ueptr>()-q.npw2 as ueptr));
        debug_assert!(id  < (1 << q.npw2-8));
        Einfo((gen << q.npw2) | (id << 8) | (npw2 as ueptr))
    }

    fn npw2(&self) -> u8 {
        (self.0 & 0xff) as u8
    }

    fn size(&self) -> usize {
        // note this does not include metadata overhead
        (Eptr::ALIGN << self.npw2()) as usize
    }

    fn gen(&self, q: &Equeue) -> ueptr {
        self.0 >> q.npw2
    }

    fn id(&self, q: &Equeue) -> ueptr {
        let mask = (1 << q.npw2) - 1;
        (self.0 & mask) >> 8
    }

    fn inc_gen(&self, q: &Equeue) -> Einfo {
        Einfo(self.0.wrapping_add(1 << q.npw2))
    }

    fn inc_id(&self, q: &Equeue) -> Einfo {
        let mask = (1 << q.npw2) - 1;
        Einfo((self.0 & !mask) | (self.0.wrapping_add(1 << 8) & mask))
    }

    fn inc_both(&self, q: &Equeue) -> Einfo {
        // we don't really care about the extra inc from overflowing the id here
        let mask = (1 << q.npw2) - 1;
        Einfo(self.0.wrapping_add((1 << q.npw2) + (1 << 8)))
    }
}

/// Internal event header
#[derive(Debug)]
struct Ebuf {
    // info contains mark + npw2 + bits, and since mark <= width/2,
    // we know this at least gives us 2 bytes, even when usize == u32
    info: Atomic<Einfo>,
    next: Atomic<Eptr>,
    sibling: Atomic<Eptr>,

    cb: Option<fn(*mut u8)>,
    drop: Option<fn(*mut u8)>,
    target: utick,
    period: itick,
}

impl Ebuf {
    // info access
    fn npw2(&self) -> u8 {
        // the npw2 field never actually changes
        unsafe { &*(&self.info as *const _ as *const Einfo) }.npw2()
    }

    fn size(&self) -> usize {
        // the npw2 field never actually changes
        unsafe { &*(&self.info as *const _ as *const Einfo) }.size()
    }

    fn gen(&self, q: &Equeue) -> ueptr {
        self.info.load().gen(q)
    }

    fn id(&self, q: &Equeue) -> ueptr {
        self.info.load().id(q)
    }

    fn inc_gen(&self, q: &Equeue) {
        self.info.store(self.info.load().inc_gen(q))
    }

    fn inc_id(&self, q: &Equeue) {
        self.info.store(self.info.load().inc_id(q))
    }

    fn inc_both(&self, q: &Equeue) {
        self.info.store(self.info.load().inc_both(q))
    }

    // access to the trailing buffer
    fn data_ptr<T>(&self) -> *const T {
        unsafe { (self as *const Ebuf).add(1) as *const T }
    }

    fn data_mut_ptr<T>(&mut self) -> *mut T {
        unsafe { (self as *mut Ebuf).add(1) as *mut T }
    }

    unsafe fn data_ref<'a, T>(&'a self) -> &'a T {
        &*self.data_ptr()
    }

    unsafe fn data_mut<'a, T>(&'a mut self) -> &'a mut T {
        &mut *self.data_mut_ptr()
    }

    unsafe fn from_data_mut_ptr<'a, T>(ptr: *mut T) -> Option<&'a mut Ebuf> {
        if !ptr.is_null() {
            Some(&mut *(ptr as *mut Ebuf).sub(1))
        } else {
            None
        }
    }
}

/// Slab-internal pointer + the queue it's on
///
/// Since we need the queue to decode the generation count, it's convenient
/// to have a type that combines these. This is pretty large for just a pointer,
/// but should only exist on stacks (and get optimized away?)
#[derive(Copy, Clone)]
struct Eref<'a>{
    q: &'a Equeue,
    eptr: Eptr,
    e: &'a Ebuf,
}

impl Debug for Eref<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Eref")
            .field(&format_args!("gen {}", self.eptr_gen()))
            .field(self.e)
            .finish()
    }
}

impl<'a> Eref<'a> {
    unsafe fn from_parts(
        q: &'a Equeue,
        gen: ueptr,
        e: &'a Ebuf
    ) -> Eref<'a> {
        Eref {
            q: q,
            eptr: Eptr::from_parts(q, gen, e),
            e: e
        }
    }

    fn from_ebuf(q: &'a Equeue, e: &'a Ebuf) -> Eref<'a> {
        Eref {
            q: q,
            eptr: Eptr::from(q, e),
            e: e,
        }
    }

    fn from_eptr(q: &'a Equeue, eptr: Eptr) -> Option<Eref<'a>> {
        eptr.as_ref(q)
            .map(|e| Eref {
                q: q,
                eptr: eptr,
                e: e,
            })
    }

    // we can "claim" an Ebuf once we remove it from shared structures,
    // the claim is unsafe, but afterwards we can leverage Rust's type
    // system to know whether or not we have exclusive access to the Ebuf
    unsafe fn claim(&self) -> Emut<'a> {
        Emut {
            q: self.q,
            eptr: self.eptr,
            e: &mut *(self.e as *const Ebuf as *mut Ebuf)
        }
    }

    fn as_ptr(&self) -> *const Ebuf {
        self.e as *const Ebuf
    }

    fn as_eptr(&self) -> Eptr {
        self.eptr
    }

    fn eptr_gen(&self) -> ueptr {
        self.eptr.gen(self.q)
    }

    fn gen(&self) -> ueptr {
        self.e.gen(self.q)
    }

    fn id(&self) -> ueptr {
        self.e.id(self.q)
    }

    fn inc_gen(&mut self) {
        self.e.inc_gen(self.q);
        self.eptr = self.eptr.inc(self.q);
    }

    fn inc_id(&mut self) {
        self.e.inc_id(self.q);
    }

    fn inc_both(&mut self) {
        self.e.inc_both(self.q);
        self.eptr = self.eptr.inc(self.q);
    }

    fn is_removing(&self) -> bool {
        self.eptr_gen() != self.gen()
    }
}

impl<'a> Deref for Eref<'a> {
    type Target = &'a Ebuf;
    fn deref<'b>(&'b self) -> &'b &'a Ebuf {
        &self.e
    }
}

trait OptionEref {
    fn as_ptr(&self) -> *const Ebuf;
}

impl OptionEref for Option<Eref<'_>> {
    fn as_ptr(&self) -> *const Ebuf {
        match self.as_ref() {
            Some(e) => e.as_ptr(),
            None => ptr::null(),
        }
    }
}

/// Slab-internal pointer + the queue it's on, but _mutable_
struct Emut<'a>{
    q: &'a Equeue,
    eptr: Eptr,
    e: &'a mut Ebuf,
}

impl Debug for Emut<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Emut")
            .field(&format_args!("gen {}", self.eptr_gen()))
            .field(self.e)
            .finish()
    }
}

impl<'a> Emut<'a> {
    unsafe fn from_parts(
        q: &'a Equeue,
        gen: ueptr,
        e: &'a mut Ebuf
    ) -> Emut<'a> {
        Emut {
            q: q,
            eptr: Eptr::from_parts(q, gen, e),
            e: e
        }
    }

    fn from_ebuf(q: &'a Equeue, e: &'a mut Ebuf) -> Emut<'a> {
        Emut {
            q: q,
            eptr: Eptr::from(q, e),
            e: e,
        }
    }

    unsafe fn from_data_mut_ptr<T>(q: &'a Equeue, ptr: *mut T) -> Option<Emut<'a>> {
        if let Some(e) = Ebuf::from_data_mut_ptr(ptr) {
            Some(Emut::from_ebuf(q, e))
        } else {
            None
        }
    }

    fn as_eref(&'a self) -> Eref<'a> {
        Eref {
            q: self.q,
            eptr: self.eptr,
            e: self.e
        }
    }

    fn as_ptr(&self) -> *const Ebuf {
        self.e as *const Ebuf
    }

    fn eptr(&self) -> Eptr {
        self.eptr
    }

    fn eptr_gen(&self) -> ueptr {
        self.eptr.gen(self.q)
    }

    fn gen(&self) -> ueptr {
        self.e.gen(self.q)
    }

    fn id(&self) -> ueptr {
        self.e.id(self.q)
    }

    fn inc_gen(&mut self) {
        self.e.inc_gen(self.q);
        self.eptr = self.eptr.inc(self.q);
    }

    fn inc_id(&mut self) {
        self.e.inc_id(self.q);
    }

    fn inc_both(&mut self) {
        self.e.inc_both(self.q);
        self.eptr = self.eptr.inc(self.q);
    }

    fn is_removing(&self) -> bool {
        self.eptr_gen() != self.gen()
    }
}

impl<'a> Deref for Emut<'a> {
    type Target = &'a mut Ebuf;
    fn deref<'b>(&'b self) -> &'b &'a mut Ebuf {
        &self.e
    }
}

impl<'a> DerefMut for Emut<'a> {
    fn deref_mut<'b>(&'b mut self) -> &'b mut &'a mut Ebuf {
        &mut self.e
    }
}

trait OptionEmut {
    fn as_ptr(&self) -> *const Ebuf;
}

impl OptionEmut for Option<Emut<'_>> {
    fn as_ptr(&self) -> *const Ebuf {
        match self.as_ref() {
            Some(e) => e.as_ptr(),
            None => ptr::null(),
        }
    }
}

impl Atomic<Eptr> {
    fn load_eref<'a>(&self, q: &'a Equeue) -> Option<Eref<'a>> {
        Eref::from_eptr(q, self.load())
    }

    fn store_eref<'a>(&self, e: Option<Eref<'a>>) {
        self.store(match e {
            Some(e) => e.as_eptr(),
            None => Eptr::null(),
        })
    }
}

//impl Equeue {
//    // Eptr interactions
//    fn eptr_from<'a>(
//        &'a self,
//        gen: usize,
//        e: Option<&'a Ebuf>
//    ) -> Eptr {
//        let off = match e {
//            Some(e) => unsafe {
//                (e as *const Ebuf as *const u8)
//                    .offset_from(self.slab.as_ptr())
//                    as usize
//                    / Eptr::ALIGN
//            },
//            None => 0,
//        };
//
//        // make sure our gen/off aren't overflowing
//        debug_assert!(gen <= (1 << self.gnpw2));
//        debug_assert!(off <= (1 << self.enpw2));
//
//        Eptr((gen << self.enpw2) | off)
//    }
//
//    fn gen_inc(&self, gen: usize) -> usize {
//        // we need to mask properly
//        let mask = (1 << self.gnpw2) - 1;
//        gen.wrapping_add(1) & mask
//    }
//
//    fn eptr_mark(&self, eptr: Eptr) -> usize {
//        eptr.0 >> self.gnpw2+self.enpw2
//    }
//
//    fn eptr_set_mark(&self, eptr: Eptr, mark: usize) -> Eptr {
//        debug_assert!(mark <= (1 << self.gnpw2));
//        let mask = (1 << self.gnpw2+self.enpw2) - 1;
//        Eptr((mark << self.gnpw2+self.enpw2) | (eptr.0 & mask))
//    }
//
//    fn eptr_inc_mark(&self, eptr: Eptr) -> Eptr {
//        Eptr(eptr.0.wrapping_add(1 << self.gnpw2+self.enpw2))
//    }
//
//    fn eptr_cp_mark(&self, eptr: Eptr, marked: Eptr) -> Eptr {
//        let mask = (1 << self.gnpw2+self.enpw2) - 1;
//        Eptr((marked.0 & !mask) | (eptr.0 & mask))
//    }
//
//    fn eptr_gen(&self, eptr: Eptr) -> usize {
//        let gmask = (1 << self.gnpw2) - 1;
//        (eptr.0 >> self.enpw2) & gmask
//    }
//
//    fn eptr_set_gen(&self, eptr: Eptr, gen: usize) -> Eptr {
//        debug_assert!(gen <= (1 << self.gnpw2));
//        let mask = !(((1 << self.gnpw2) - 1) << self.enpw2);
//        Eptr((gen << self.enpw2) | (eptr.0 & mask))
//    }
//
//    fn eptr_inc_gen(&self, eptr: Eptr) -> Eptr {
//        Eptr(
//            eptr.0
//                .rotate_left(8*size_of::<Eptr>() as u32 - (self.gnpw2+self.enpw2) as u32)
//                .wrapping_add(1 << 8*size_of::<Eptr>() as u32 - self.gnpw2 as u32)
//                .rotate_right(8*size_of::<Eptr>() as u32 - (self.gnpw2+self.enpw2) as u32)
//        )
//    }
//
//    fn eptr_as_ptr(&self, eptr: Eptr) -> *const Ebuf {
//        let mask = (1 << self.enpw2) - 1;
//        if eptr.0 & mask != 0 {
//            &self.slab[(eptr.0 & mask) * Eptr::ALIGN]
//                as *const u8 as *const Ebuf
//        } else {
//            ptr::null()
//        }
//    }
//
//    unsafe fn eptr_as_mut_ptr(&self, eptr: Eptr) -> *mut Ebuf {
//        self.eptr_as_ptr(eptr) as *mut Ebuf
//    }
//
//    fn eptr_as_ref<'a>(&'a self, eptr: Eptr) -> Option<&'a Ebuf> {
//        unsafe { self.eptr_as_ptr(eptr).as_ref() }
//    }
//
//    unsafe fn eptr_as_mut<'a>(&'a self, eptr: Eptr) -> Option<&'a mut Ebuf> {
//        self.eptr_as_mut_ptr(eptr).as_mut()
//    }
//
//    // Cas<Eptr> interactions
//    fn eptr_load<'a>(&'a self, eptr: &Cas<Eptr>) -> Option<&'a Ebuf> {
//        self.eptr_as_ref(eptr.load())
//    }
//
//    fn eptr_load_ex<'a>(
//        &'a self,
//        eptr: &mut Cas<Eptr>
//    ) -> Option<&'a mut Ebuf> {
//        unsafe { self.eptr_as_mut(eptr.load_ex()) }
//    }
//}
//
//impl Ebuf {
//    // info access
//    fn npw2(&self) -> u8 {
//        (self.info & 0xff) as u8
//    }
//
//    fn size(&self) -> usize {
//        // note this does not include metadata overhead
//        Eptr::ALIGN << self.npw2()
//    }
//
//    // access to the trailing buffer
//    unsafe fn as_ptr<T>(&self) -> *const T {
//        (self as *const Ebuf).add(1) as *const T
//    }
//
//    unsafe fn as_mut_ptr<T>(&mut self) -> *mut T {
//        (self as *mut Ebuf).add(1) as *mut T
//    }
//
//    fn as_ref<'a, T>(&'a self) -> &'a T {
//        unsafe { &*self.as_ptr() }
//    }
//
//    fn as_mut<'a, T>(&'a mut self) -> &'a mut T {
//        unsafe { &mut *self.as_mut_ptr() }
//    }
//
//    unsafe fn from_mut_ptr<'a, T>(ptr: *mut T) -> Option<&'a mut Ebuf> {
//        if !ptr.is_null() {
//            Some(&mut *(ptr as *mut Ebuf).sub(1))
//        } else {
//            None
//        }
//    }
//}
//
//impl Equeue {
//    // we can "claim" an Ebuf once we remove it from shared structures,
//    // the claim is unsafe, but afterwards we can leverage Rust's type
//    // system to know whether or not we have exclusive access to the Ebuf
//    unsafe fn ebuf_claim<'a>(&'a self, e: &'a Ebuf) -> &'a mut Ebuf {
//        &mut *(e as *const Ebuf as *mut Ebuf)
//    }
//
//    fn ebuf_id(&self, e: &Ebuf) -> usize {
//        self.eptr_mark(Eptr(e.info))
//    }
//
//    fn ebuf_inc_id(&self, e: &mut Ebuf) {
//        e.info = self.eptr_inc_mark(Eptr(e.info)).0
//    }
//
//    // access the generation count embedded in our event's mark
//    fn ebuf_gen_load<'a>(&'a self, e: &'a Ebuf) -> usize {
//        self.eptr_mark(e.next.load())
//    }
//
//    fn ebuf_gen_load_ex<'a>(&'a self, e: &'a mut Ebuf) -> usize {
//        self.eptr_mark(e.next.load_ex())
//    }
//
//    fn ebuf_gen_inc_ex<'a>(&'a self, e: &'a mut Ebuf) {
//        let next = self.eptr_inc_mark(e.next.load_ex());
//        e.next.store_ex(next);
//    }
//}

/// Event queue struct
#[derive(Debug)]
pub struct Equeue {
    // memory management
    slab: &'static [u8],
    slab_front: Atomic<ueptr>,
    slab_back: Atomic<ueptr>,
    npw2: u8,

    // queue management
    queue: Atomic<Eptr>,

    // other things
    clock: DefaultClock,
    lock: DefaultLock,
    sema: DefaultSema,
}

// TODO can we assert that these are already satisfied?
unsafe impl Send for Equeue {}
unsafe impl Sync for Equeue {}

impl Equeue {
    pub fn with_buffer(buffer: &'static mut [u8]) -> Result<Equeue, Error> {
        // align buffer
        let align = alignup(buffer.as_ptr() as usize, Eptr::ALIGN)
            - buffer.as_ptr() as usize;
        let buffer = match buffer.get_mut(align..) {
            // already out of memory?
            Some(buffer) => buffer,
            None => return Err(Error::NoMem),
        };
        let front = 0;
        let back = ueptr::try_from(buffer.len()).map_err(|_| Error::Overflow)?;

        // go ahead and zero our buffer, this makes it easier to manage bucket
        // allocation, which needs to be null the moment a bucket is allocated
        buffer.fill(0);

        // find the maximum mark/gen field size in our eptrs, these are crammed
        // next to the offset, which we make as small as possible by leveraging
        // alignment and limiting references to our memory region
        //
        // but we also have to make sure we can fit event ids+npw2 fields in
        // this, it's somewhat arbitrary but we make this at least 8-bits each
        let npw2 = max(npw2(buffer.len().saturating_sub(Eptr::ALIGN)), 2*8);

        // make sure there is some minimum generation width, but it's hard to
        // know what this needs to be...
        assert!(8*size_of::<ueptr>() - npw2 as usize >= 8);
        assert!(npw2-8 >= 8);

        Ok(Equeue {
            slab: buffer,
            slab_front: Atomic::new(front),
            slab_back: Atomic::new(back),
            npw2: npw2,

            queue: Atomic::new(Eptr::null()),

            clock: DefaultClock::new(),
            lock: DefaultLock::new(),
            sema: DefaultSema::new(),
        })
    }

    pub fn now(&self) -> utick {
        self.clock.now()
    }

    fn contains_ebuf(&self, e: &Ebuf) -> bool {
        self.slab.as_ptr_range()
            .contains(&(e.deref() as *const _ as *const u8))
    }

    fn buckets<'a>(&'a self) -> &'a [Atomic<Eptr>] {
        let slab_front = self.slab_front.load();
        unsafe {
            slice::from_raw_parts(
                self.slab.as_ptr() as *const Atomic<Eptr>,
                slab_front / size_of::<Eptr>()
            )
        }
    }

    // Memory management
    fn alloc_ebuf<'a>(&'a self, layout: Layout) -> Result<Emut<'a>, Error> {
        assert!(layout.align() <= Eptr::ALIGN);

        // find best bucket
        let npw2 = npw2((layout.size()+Eptr::ALIGN-1) / Eptr::ALIGN);

        // do we have an allocation in our buckets? we don't look
        // at larger buckets because those are likely to be reused, we don't
        // want to starve larger events with smaller events
        if let Some(bucket) = self.buckets().get(npw2 as usize)
            // don't bother locking if bucket is empty
            .filter(|bucket| bucket.load_eref(self).is_some())
        {
            // try to take an event from a bucket
            let e = self.lock.lock(|| {
                // this could still fail
                if let Some(e) = bucket.load_eref(self) {
                    bucket.store_eref(e.sibling.load_eref(self));
                    Some(e)
                } else {
                    None
                }
            });

            if let Some(e) = e {
                let mut e = unsafe { e.claim() };

                // zero certain fields
                e.cb = None;
                e.drop = None;
                e.target = 0;
                e.period = -1;
                return Ok(e);
            }
        }

        let new_slab_back = loop {
            // check if we even have enough memory available, we allocate both
            // an event and maybe some buckets if we don't have enough
            let slab_front = self.slab_front.load();
            let slab_back = self.slab_back.load();
            let new_slab_front = max(
                (npw2 as usize + 1)*size_of::<Eptr>(),
                slab_front
            );
            let new_slab_back = aligndown(
                slab_back.saturating_sub(size_of::<Ebuf>() + (Eptr::ALIGN << npw2)),
                Eptr::ALIGN
            );

            if new_slab_front > new_slab_back {
                return Err(Error::NoMem);
            }

            // try to actually commit our allocation, if slab_front/slab_back
            // changed already, we just try again, someone should be making
            // progress if that happens
            //
            // TODO just use a guard
            if self.lock.lock(|| {
                if self.slab_front.load() == slab_front
                    && self.slab_back.load() == slab_back
                {
                    if new_slab_front > slab_front {
                        self.slab_front.store(new_slab_front);
                    }

                    debug_assert!(new_slab_back < slab_back);
                    self.slab_back.store(new_slab_back);

                    true
                } else {
                    false
                }
            }) {
                break new_slab_back;
            }
        };

        unsafe {
            let e = &self.slab[new_slab_back] as *const u8 as *const Ebuf as *mut Ebuf;
            e.write(Ebuf {
                info: Atomic::new(Einfo::new(self, 0, 0, npw2)),
                next: Atomic::new(Eptr::null()),
                sibling: Atomic::new(Eptr::null()),

                cb: None,
                drop: None,
                target: 0,
                period: -1,
            });

            Ok(Eref::from_parts(self, 0, &mut *e).claim())
        }
    }

    fn dealloc_ebuf(&self, e: Emut) {
        debug_assert!(self.contains_ebuf(&e));
        let mut e = e;

        // we can load buckets here because it can never shrink
        let bucket = &self.buckets()[e.npw2() as usize];

        // give our event a new id/generation count
        e.inc_both();

        // add our event to a bucket, while also incrementing our
        // generation count
        let e = e.as_eref();
        self.lock.lock(|| {
            // push onto bucket
            let sibling = bucket.load_eref(self);
            debug_assert_ne!(e.as_ptr(), sibling.as_ptr());
            e.sibling.store_eref(bucket.load_eref(self));
            bucket.store_eref(Some(e));
        })
    }

//    // Queue management
//    fn enqueue_ebuf(&self, e: &mut Ebuf, target: utick) -> Eptr {
//        debug_assert!(e.cb.is_some());
//        e.target = target;
//        let eptr = self.eptr_from(self.ebuf_id(e), Some(e));
//
//        'retry: loop {
//            let mut headptr = self.queue.load();
//            let mut headsrc = &self.queue;
//
//            // find insertion point
//            let mut delta = Ordering::Greater;
//            let mut nextptr = Eptr::null();
//            while let Some(head) = self.eptr_as_ref(headptr) {
//                // generation mismatch? someone is trying to remove and we
//                // need to help out
//                nextptr = head.next.load();
//                if self.eptr_mark(nextptr) != self.eptr_gen(headptr) {
//                    let nextptr = self.eptr_set_mark(nextptr, self.eptr_mark(headptr));
//                    if let Err(_) = headsrc.cas(headptr, nextptr) {
//                        // someone else removed the event for us, but we don't know
//                        // what else has happened, so we need to retry
//                        continue 'retry;
//                    }
//
//                    // if no one fixed this before us, we can continue
//                    headptr = nextptr;
//                    continue;
//                }
//
//                let ndelta = scmp(head.target, target);
//                if ndelta.is_ge() {
//                    delta = ndelta;
//                    break;
//                }
//
//                headptr = nextptr;
//                headsrc = &head.next;
//            };
//
//            // set ourselves up for insertion
//            let siblingptr;
//            let eptr;
//            match delta {
//                Ordering::Greater => {
//                    // inserting a new slice, nothing complicated here
//                    let gen = self.eptr_mark(e.next.load_ex());
//                    eptr = self.eptr_cp_mark(self.eptr_from(gen, Some(e)), headptr);
//                    nextptr = self.eptr_set_mark(headptr, gen);
//                    siblingptr = self.eptr_cp_mark(Eptr::null(), e.sibling.load_ex());
//                }
//                Ordering::Equal => {
//                    // inserting onto an existing slice, which is a bit complicated
//                    //
//                    // We can't really replace the top event atomically, so instead
//                    // we insert our event _after_ the existing slice, but with the
//                    // existing slice in our sibling list, while also marking the
//                    // existing slice to be removed. This creates duplicate
//                    // entries, but as long as we fix removals before dispatching
//                    // events things work out.
//                    let predptr = headptr;
//                    headptr = nextptr;
//                    headsrc = &self.eptr_as_ref(predptr).unwrap().next;
//
//                    let gen = self.eptr_mark(e.next.load_ex());
//                    eptr = self.eptr_cp_mark(self.eptr_from(gen, Some(e)), self.eptr_inc_mark(headptr));
//                    nextptr = self.eptr_set_mark(headptr, gen);
//                    // TODO probably a better way to do this gen inc
//                    siblingptr = self.eptr_cp_mark(self.eptr_inc_gen(predptr), e.sibling.load_ex());
//                }
//                Ordering::Less => unreachable!(),
//            };
//            debug_assert_ne!(e as *const _, self.eptr_as_ptr(nextptr));
//            debug_assert_ne!(e as *const _, self.eptr_as_ptr(siblingptr));
//            e.next.store_ex(nextptr);
//            e.sibling.store_ex(siblingptr);
//
//            // CAS try to insert our event into the queue
//            if let Err(_) = headsrc.cas(headptr, eptr) {
//                // if we fail here any number of things could have happened,
//                // we need to completely restart
//                continue 'retry;
//            }
//
//            // In _theory_, if we were pushing onto an existing slice, we should
//            // go back and clean up the duplicate entries we created.
//            //
//            // But this gets complicated, and in practice, since any access to
//            // the queue must fix pending removes, the duplicate entries will
//            // get fixed before they have any effect on either behavior or
//            // runtime
//            break;
//        }
//
//        eptr
//    }
//
//    fn unqueue_ebuf<'a>(&'a self, e: &'a Ebuf) {
//        // try to find the event and reclaim the memory, this can always
//        // fail if the event is already prepared for dispatch, and it can
//        // be in this state for however long any callbacks block for
//
//        // we actually only care about the target, we clean any events we
//        // come across since it's necessary to keep the queue in a stable
//        // state
//        let target = e.target;
//        let mut defered_reclaim = None;
//        'retry: loop {
//            let mut headptr = self.queue.load();
//            let mut headsrc = &self.queue;
//
//            // find target slice
//            let mut delta = Ordering::Greater;
//            let mut nextptr = Eptr::null();
//            while let Some(head) = self.eptr_as_ref(headptr) {
//                // generation mismatch? someone is trying to remove and we
//                // need to help out
//                nextptr = head.next.load();
//                if self.eptr_mark(nextptr) != self.eptr_gen(headptr) {
//                    let nextptr = self.eptr_set_mark(nextptr, self.eptr_mark(headptr));
//                    if let Err(_) = headsrc.cas(headptr, nextptr) {
//                        // someone else removed the event for us, but we don't know
//                        // what else has happened, so we need to retry
//                        continue 'retry;
//                    }
//
//                    // if no one fixed this before us, we can continue
//                    headptr = nextptr;
//                    continue;
//                }
//
//                let ndelta = scmp(head.target, target);
//                if ndelta.is_ge() {
//                    delta = ndelta;
//                    break;
//                }
//
//                headptr = nextptr;
//                headsrc = &head.next;
//            };
//
//            {
//                if let Some(head) = self.eptr_as_ref(headptr) {
//                    let mut headptr = head.sibling.load();
//                    let mut headsrc = &head.sibling;
//                    while let Some(head) = self.eptr_as_ref(headptr) {
//                        // generation mismatch? someone is trying to remove and we
//                        // need to help out
//                        let id = self.ebuf_id(head);
//                        let siblingptr = head.sibling.load();
//                        if self.eptr_mark(siblingptr) != id {
//                            let siblingptr = self.eptr_set_mark(siblingptr, self.eptr_mark(headptr));
//                            if let Err(_) = headsrc.cas(headptr, siblingptr) {
//                                // someone else removed the event for us, but we don't know
//                                // what else has happened, so we need to retry
//                                continue 'retry;
//                            }
//
//                            // if no one fixed this before us, we can continue
//                            headptr = siblingptr;
//                            continue;
//                        }
//
//                        let ndelta = scmp(head.target, target);
//                        if ndelta.is_ge() {
//                            delta = ndelta;
//                            break;
//                        }
//
//                        headptr = siblingptr;
//                        headsrc = &head.sibling;
//                    };
//                }
//            }
//
//            if let Some(reclaimme) = defered_reclaim {
//                let e = unsafe { self.ebuf_claim(reclaimme) };
//                if let Some(drop) = e.drop {
//                    drop(unsafe { e.as_mut_ptr() });
//                }
//                self.dealloc_ebuf(e);
//
//                defered_reclaim = None
//            }
//
//            // clean up any canceled events
//            if let (Ordering::Equal, Some(head))
//                = (delta, self.eptr_as_ref(headptr))
//            {
//                let id = self.ebuf_id(head);
//                let siblingptr = head.sibling.load();
//                match (
//                    self.eptr_mark(siblingptr) != id,
//                    self.eptr_as_ref(siblingptr)
//                ) {
//                    (true, Some(sibling)) => {
//                        // found head of slice with siblings, in order to
//                        // mark this for removal, we need to move the siblings
//                        // next to us, fortunately we can do this while marking
//                        // ourself for removal atomically
//
//                        // prepare sibling for reinsertion
//                        let oldsiblingnextptr = sibling.next.load();
//                        if self.eptr_mark(oldsiblingnextptr) != self.eptr_gen(siblingptr) {
//                            // we may have already been dispatched by now
//                            continue 'retry;
//                        }
//                        let siblingnextptr = self.eptr_cp_mark(
//                            nextptr,
//                            oldsiblingnextptr
//                        );
//                        debug_assert_ne!(sibling as *const _, self.eptr_as_ptr(siblingnextptr));
//                        // TODO restructure a bit?
//                        if oldsiblingnextptr != siblingnextptr {
//                            if let Err(_) = sibling.next.cas(oldsiblingnextptr, siblingnextptr) {
//                                // we may have already been dispatched by now
//                                continue 'retry;
//                            }
//                        }
//
//                        // mark for removal and insert sibling
//                        let dirty_nextptr = self.eptr_inc_mark(self.eptr_cp_mark(
//                            siblingptr,
//                            nextptr
//                        ));
//                        debug_assert_ne!(head as *const _, self.eptr_as_ptr(dirty_nextptr));
//                        if let Err(_) = head.next.cas(nextptr, dirty_nextptr) {
//                            continue 'retry;
//                        }
//
//                        // we need to make sure the event is out of the queue
//                        // before we can reclaim the memory
//                        debug_assert!(defered_reclaim.is_none());
//                        defered_reclaim = Some(head);
//                        continue 'retry;
//                    }
//                    (true, None) => {
////                        // found head of slice with no sibling, we can just
////                        // mark ourself for removal
////                        let dirty_nextptr = self.eptr_inc_mark(nextptr);
////                        if let Err(x) = head.next.cas(nextptr, dirty_nextptr) {
////                            continue 'retry;
////                        }
////
////                        // we need to make sure the event is out of the queue
////                        // before we can reclaim the memory
////                        debug_assert!(defered_reclaim.is_none());
////                        defered_reclaim = Some(head);
////                        continue 'retry;
//                    }
//                    (false, _) => {
//                    }
//                }
//            }
//
//            break;
//        }
//    }
//
//    fn dequeue_ebufs<'a>(&'a self, now: utick) -> Result<&'a Ebuf, itick> {
//        let mut dequeuedptr = Cas::new(Eptr::null());
//        let mut dequeuedtail = &dequeuedptr;
//        let delta = 'retry: loop {
//            let mut headptr = self.queue.load();
//            while let Some(head) = self.eptr_as_ref(headptr) {
//                // generation mismatch? someone is trying to remove and we
//                // need to help out
//                let nextptr = head.next.load();
//                if self.eptr_mark(nextptr) != self.eptr_gen(headptr) {
//                    let nextptr = self.eptr_set_mark(nextptr, self.eptr_mark(headptr));
//                    if let Err(_) = self.queue.cas(headptr, nextptr) {
//                        // someone else removed the event for us, but we don't know
//                        // what else has happened, so we need to retry
//                        continue 'retry;
//                    }
//
//                    // if no one fixed this before us, we can continue
//                    headptr = nextptr;
//                    continue;
//                }
//
//                // is this slice ready to dispatch?
//                let delta = sdiff(head.target, now);
//                if delta > 0 {
//                    // no? return how long until the next event
//                    break 'retry delta;
//                }
//
//                // ok, the slice is ready to dispatch
//                //
//                // CAS 1. mark the slice for removal
//                let dirty_nextptr = self.eptr_inc_mark(nextptr);
//                if let Err(_) = head.next.cas(nextptr, dirty_nextptr) {
//                    continue 'retry;
//                }
//
//                // we've effectively removed the slice, but can't claim the
//                // memory yet
//                //
//                // but, if we continue around this loop until all events sooner
//                // then our target have been processed, we can be sure that the
//                // event will actually be removed
//                
//                // to actually keep track of all these events, stick them on
//                // an ad-hoc queue, keep in mind that the sibling pointers never
//                // change once an event is enqueued
//                //
//                // note that we're also inverting our slices, which act sort of
//                // like stacks and are stored in reverse order
//                let mut predptr = Eptr::null();
//                let mut headptr = headptr;
//                let mut head = head;
//                let ndequeuedtail = &head.sibling;
//                loop {
//                    let mut siblingptr = head.sibling.load();
//                    loop {
//                        let predptr = self.eptr_cp_mark(predptr, siblingptr);
//                        if let Err(x) = head.sibling.cas(siblingptr, predptr) {
//                            // this can only happen if an event is canceled
//                            siblingptr = x;
//                            continue;
//                        }
//                        break;
//                    }
//
//                    let sibling = match self.eptr_as_ref(siblingptr) {
//                        Some(sibling) => sibling,
//                        None => break,
//                    };
//
//                    predptr = headptr;
//                    headptr = siblingptr;
//                    head = sibling;
//                }
//
//                // stick on dequeued list
//                let mut dequeuedtailptr = dequeuedtail.load();
//                loop {
//                    debug_assert_eq!(self.eptr_as_ptr(dequeuedtailptr), ptr::null());
//                    let headptr = self.eptr_cp_mark(headptr, dequeuedtailptr);
//                    if let Err(x) = dequeuedtail.cas(dequeuedtailptr, headptr) {
//                        // this can only happend if an event is canceled
//                        dequeuedtailptr = x;
//                        continue;
//                    }
//                    break;
//                }
//                dequeuedtail = ndequeuedtail;
//
//                // continue to fix remove/find more slices
//                continue 'retry;
//            }
//
//            // nothing to do I guess
//            break 'retry -1;
//        };
//
//        // did we find any slices to dequeue?
//        match self.eptr_as_ref(dequeuedptr.load_ex()) {
//            Some(dequeued) => Ok(dequeued),
//            None => Err(delta),
//        }
//    }
//
//    // Central post function
//    fn post_ebuf(&self, e: &mut Ebuf, target: utick) -> Eptr {
//        let eptr = self.enqueue_ebuf(e, target);
//
//        // signal queue has changed
//        self.sema.signal();
//
//        eptr
//    }
//
//    // Central dispatch function
//    pub fn dispatch(&self, ticks: itick) {
//        // get the current time
//        let mut now = self.clock.now();
//        let timeout = now.wrapping_add(ticks as u64);
//
//        loop {
//            // get a slice to dispatch
//            let mut slice = self.dequeue_ebufs(now).ok();
//
//            while let Some(e) = slice {
//                // last chance to cancel
//                let id = self.ebuf_id(e);
//                let mut siblingptr = e.sibling.load();
//                let canceled = loop {
//                    // already canceled?
//                    if self.eptr_mark(siblingptr) != id {
//                        break true;
//                    }
//
//                    // if we're periodic, we actually don't mark the event as
//                    // complete, since periodic events are always cancelable
//                    if e.period >= 0 {
//                        break false;
//                    }
//
//                    // mark event as complete
//                    let dirty_siblingptr = self.eptr_inc_mark(siblingptr);
//                    if let Err(x) = e.sibling.cas(siblingptr, dirty_siblingptr) {
//                        siblingptr = x;
//                        continue;
//                    }
//
//                    break false;
//                };
//
//                // move to next event, claim current event
//                slice = self.eptr_load(&e.sibling);
//                let e = unsafe { self.ebuf_claim(e) };
//
//                if !canceled {
//                    // dispatch!
//                    e.cb.unwrap()(unsafe { e.as_mut_ptr() });
//                }
//
//                if !canceled && e.period >= 0 {
//                    // reenqueue?
//                    self.enqueue_ebuf(
//                        e,
//                        self.clock.now().wrapping_add(e.period as u64)
//                    );
//                } else {
//                    // call drop, return event to memory pool
//                    if let Some(drop) = e.drop {
//                        drop(unsafe { e.as_mut_ptr() });
//                    }
//                    self.dealloc_ebuf(e);
//                }
//            }
//
//            // should we stop dispatching?
//            //
//            // note that time could have changed _significantly_
//            now = self.clock.now();
//            let timeout_left = sdiff(timeout, now);
//            if ticks >= 0 && timeout_left <= 0 {
//                return;
//            }
//
//            // ok how long should we sleep for
//            //
//            // Note that we always try to sleep between slices, this is
//            // just to behave nicely in case the system's semaphore implementation
//            // does something "clever". Note we also never enter here if
//            // ticks is 0 for similar reasons.
//            let mut delay = match self.eptr_as_ref(self.queue.load()) {
//                Some(head) => max(sdiff(head.target, now), 0),
//                None => -1,
//            };
//
//            if (delay as utick) > (timeout_left as utick) {
//                delay = timeout_left;
//            }
//
//            self.sema.wait(delay);
//
//            // update current time
//            now = self.clock.now();
//        }
//    }
//
//    // Central cancel function
//    pub fn cancel(&self, id: Id) -> bool {
//        let eptr = Eptr::from(id);
//        let e = match self.eptr_as_ref(eptr)
//            // I supposed eptrs could be passed between equeues, it's probably
//            // an error to do so, but the correct action is to ignore the request
//            .filter(|e| self.contains_ebuf(e))
//        {
//            Some(e) => e,
//            None => return false,
//        };
//
//        // try to cancel by incrementing the id shoved in our sibling mark
//        let mut siblingptr = e.sibling.load();
//        loop {
//            if self.eptr_mark(siblingptr) != self.eptr_gen(eptr) {
//                // already canceled?
//                return false;
//            }
//
//            let dirty_siblingptr = self.eptr_inc_mark(siblingptr);
//            if let Err(x) = e.sibling.cas(siblingptr, dirty_siblingptr) {
//                siblingptr = x;
//                continue;
//            }
//
//            break;
//        }
//
//        // try to clean up memory
//        self.unqueue_ebuf(e);
//
//        true
//    }

    // Handling of raw allocations
    pub unsafe fn alloc_raw(&self, layout: Layout) -> *mut u8 {
        match self.alloc_ebuf(layout) {
            Ok(mut e) => e.data_mut_ptr(),
            Err(_) => ptr::null_mut(),
        }
    }

    pub unsafe fn dealloc_raw(&self, e: *mut u8, _layout: Layout) {
        debug_assert!(e.is_null() || self.contains_raw(e));
        let mut e = match Emut::from_data_mut_ptr(self, e) {
            Some(e) => e,
            None => return, // do nothing
        };

        // make sure we call drop!
        if let Some(drop) = e.drop {
            drop(e.data_mut_ptr());
        }

        // if we don't post, we need to increment our generation here
        self.dealloc_ebuf(e);
    }

    pub fn contains_raw(&self, e: *mut u8) -> bool {
        match unsafe { (e as *mut Ebuf).as_ref() } {
            Some(e) => self.contains_ebuf(e),
            None => false,
        }
    }

    pub unsafe fn set_raw_drop(&self, e: *mut u8, drop: fn(*mut u8)) {
        debug_assert!(self.contains_raw(e));
        let mut e = Emut::from_data_mut_ptr(self, e).unwrap();
        e.drop = Some(drop);
    }

    pub unsafe fn set_raw_delay(&self, e: *mut u8, delay: itick) {
        debug_assert!(self.contains_raw(e));
        let mut e = Emut::from_data_mut_ptr(self, e).unwrap();
        debug_assert!(delay >= 0);
        e.target = delay as utick;
    }

    pub unsafe fn set_raw_period(&self, e: *mut u8, period: itick) {
        debug_assert!(self.contains_raw(e));
        let mut e = Emut::from_data_mut_ptr(self, e).unwrap();
        e.period = period;
    }

//    pub unsafe fn post_raw(&self, cb: fn(*mut u8), e: *mut u8) -> Id {
//        let mut e = Ebuf::from_mut_ptr(e).unwrap();
//        debug_assert!(self.contains_ebuf(e));
//        e.cb = Some(cb);
//        let eptr = self.post_ebuf(e,
//            self.clock.now().wrapping_add(e.target)
//        );
//        Id::try_from(eptr).unwrap()
//    }
}
//
//
///// Post trait
//pub trait Post {
//    fn post(&mut self);
//}
//
//// TODO wait, should we also accept FnOnce for non-periodic events?
//impl<F: FnMut() + Send> Post for F {
//    fn post(&mut self) {
//        self()
//    }
//}
//
///// An id we can use to try to cancel an event
//#[derive(Copy, Clone)]
//pub struct Id(NonZeroUsize);
//
//impl Debug for Id {
//    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//        // these really need to be in hex to be readable
//        write!(f, "Id(0x{:x})", self.0)
//    }
//}
//
//impl TryFrom<Eptr> for Id {
//    type Error = TryFromIntError;
//    fn try_from(eptr: Eptr) -> Result<Id, Self::Error> {
//        NonZeroUsize::try_from(eptr.0).map(Id)
//    }
//}
//
//impl From<Id> for Eptr {
//    fn from(id: Id) -> Eptr {
//        Eptr(usize::from(id.0))
//    }
//}


/// Event handle
pub struct Event<'a, T>(Emut<'a>, PhantomData<T>);

impl<T: Debug> Debug for Event<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Event")
            .field(&self.0)
            .field(&self.deref())
            .finish()
    }
}

impl<'a, T> Event<'a, T> {
    fn new(e: Emut<'a>) -> Event<'a, T> {
        Event(e, PhantomData)
    }

    pub unsafe fn from_raw_parts(q: &'a Equeue, e: *mut T) -> Event<'a, T> {
        debug_assert!(q.contains_raw(e as *mut u8));
        let e = Emut::from_data_mut_ptr(q, e).unwrap();
        Event::new(e)
    }
}

impl Equeue {
    pub fn alloc<'a, T: Default>(&'a self) -> Result<Event<'a, T>, Error> {
        let mut e = self.alloc_ebuf(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(T::default()); }
        Ok(Event::new(e))
    }

    pub fn alloc_from<'a, T>(&'a self, t: T) -> Result<Event<'a, T>, Error> {
        let mut e = self.alloc_ebuf(Layout::new::<T>())?;
        unsafe { e.data_mut_ptr::<T>().write(t); }
        Ok(Event::new(e))
    }

//    // TODO should we even have this? it shouldn't be possible to pass
//    // events between unrelated equeues...
//    pub fn post<T: Post>(&self, e: Event<'_, T>) -> Id {
//        // cb/drop thunks
//        fn cb_thunk<T: Post>(e: *mut u8) {
//            unsafe { &mut *(e as *mut T) }.post();
//        }
//
//        fn drop_thunk<T>(e: *mut u8) {
//            unsafe { drop_in_place(e as *mut T) };
//        }
//
//        e.e.cb = Some(cb_thunk::<T>);
//        e.e.drop = Some(drop_thunk::<T>);
//
//        // enqueue and then forget the event, it's up to equeue to
//        // drop the event later
//        let eptr = e.q.post_ebuf(e.e,
//            self.clock.now().wrapping_add(e.e.target)
//        );
//        forget(e);
//
//        Id::try_from(eptr).unwrap()
//    }
}

impl<T> Event<'_, T> {
    pub fn delay(mut self, delay: itick) -> Self {
        debug_assert!(delay >= 0);
        self.0.target = delay as utick;
        self
    }

    pub fn period(mut self, period: itick) -> Self {
        self.0.period = period;
        self
    }

//    pub fn post(self) -> Id {
//        self.q.post(self)
//    }
}

impl<T> Drop for Event<'_, T> {
    fn drop(&mut self) {
        // make sure we clean up if the event isn't dispatched
        unsafe { drop_in_place(self.0.data_mut_ptr::<T>()) };

        // have to work around our own Emut lifetime here
        self.0.q.dealloc_ebuf(unsafe { self.0.as_eref().claim() });
    }
}

impl<T> Deref for Event<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { self.0.data_ref() }
    }
}

impl<T> DerefMut for Event<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.0.data_mut() }
    }
}

impl<T> AsRef<T> for Event<'_, T> {
    fn as_ref(&self) -> &T {
        unsafe { self.0.data_ref() }
    }
}

impl<T> AsMut<T> for Event<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { self.0.data_mut() }
    }
}

impl<T> Borrow<T> for Event<'_, T> {
    fn borrow(&self) -> &T {
        unsafe { self.0.data_ref() }
    }
}

impl<T> BorrowMut<T> for Event<'_, T> {
    fn borrow_mut(&mut self) -> &mut T {
        unsafe { self.0.data_mut() }
    }
}

//impl Equeue {
//    // convenience functions
//    pub fn call<F: Post>(&self, cb: F) -> Result<Id, Error>{
//        Ok(
//            self.alloc_from(cb)?
//                .post()
//        )
//    }
//
//    pub fn call_in<F: Post>(&self, delay: itick, cb: F) -> Result<Id, Error> {
//        Ok(
//            self.alloc_from(cb)?
//                .delay(delay)
//                .post()
//        )
//    }
//
//    pub fn call_every<F: Post>(&self, period: itick, cb: F) -> Result<Id, Error> {
//        Ok(
//            self.alloc_from(cb)?
//                .delay(period)
//                .period(period)
//                .post()
//        )
//    }
//}


#[derive(Debug, Clone)]
pub struct Usage {
    pub pending: usize,
    pub pending_bytes: usize,
    pub alloced: usize,
    pub alloced_bytes: usize,
    pub free: usize,
    pub free_bytes: usize,
    pub slices: usize,

    pub slab_total: usize,
    pub slab_fragmented: usize,
    pub slab_unused: usize,
    pub buckets: usize,
}

impl Equeue {
    pub fn usage(&self) -> Usage {
        // find slab usage
        let slab_total = self.slab.len();
        let slab_front = self.slab_front.load();
        let slab_back = self.slab_back.load();
        let slab_unused = slab_back - slab_front;

        let mut total = 0usize;
        let mut p = self.slab.get(slab_back)
            .map(|p| p as *const u8)
            .unwrap_or(ptr::null());
        while self.slab.as_ptr_range().contains(&p) {
            let e = unsafe { &*(p as *const Ebuf) };
            // the risky thing about this is we can end up with an
            // uninitialized ebuf here, which can be problematic
            if e.npw2() > npw2(self.slab.len()) {
                break;
            }

            total += 1;
            p = unsafe {
                p.add(alignup(
                    size_of::<Ebuf>() + e.size(),
                    Eptr::ALIGN
                ))
            }
        }

//        // find pending usage
//        let mut pending = 0;
//        let mut pending_bytes = 0;
//        let mut slices = 0;
//        let mut nhead = self.eptr_load(&self.queue);
//        while let Some(head) = nhead {
//            slices += 1;
//            let mut nsibling = Some(head);
//            while let Some(sibling) = nsibling {
//                pending += 1;
//                pending_bytes += size_of::<Ebuf>() + sibling.size();
//                nsibling = self.eptr_load(&sibling.sibling);
//            }
//            
//            nhead = self.eptr_load(&head.next);
//        }

        // find bucket usage
        let buckets = self.buckets();
        let mut free = 0;
        let mut free_bytes = 0;
        for (npw2, mut head) in buckets.iter().enumerate() {
            while let Some(e) = head.load_eref(self) {
                free += 1;
                free_bytes += size_of::<Ebuf>() + (Eptr::ALIGN << npw2);
                head = &e.sibling;
            }
        }

        // these are best-effort numbers, it's very easy for in-flight events
        // to create numbers that are just flat-out wrong
        //
        // we can at least clamp some of the numbers to reasonable limits
        // to avoid breaking user's code as much as possible
//        let pending = min(pending, total);
//        let pending_bytes = min(pending_bytes, slab_total.saturating_sub(slab_unused+slab_front));

        Usage {
            pending: 0,
            pending_bytes: 0,
            alloced: total.saturating_sub(free),
            alloced_bytes: slab_total.saturating_sub(free_bytes+slab_unused+slab_front),
            free: free,
            free_bytes: free_bytes+slab_unused,
            slices: 0,

//            pending: pending,
//            pending_bytes: pending_bytes,
//            alloced: total.saturating_sub(pending+free),
//            alloced_bytes: slab_total.saturating_sub(pending_bytes+free_bytes+slab_unused+slab_front),
//            free: free,
//            free_bytes: free_bytes+slab_unused,
//            slices: slices,

            slab_total: slab_total,
            slab_fragmented: slab_total.saturating_sub(slab_unused),
            slab_unused: slab_unused,
            buckets: buckets.len(),
        }
    }

    pub fn bucket_usage(&self, buckets: &mut [usize]) {
        for (bucket, mut head) in buckets.iter_mut().zip(self.buckets()) {
            let mut count = 0;
            while let Some(e) = head.load_eref(self) {
                count += 1;
                head = &e.sibling;
            }
            *bucket = count;
        }
    }

//    pub fn slice_usage(&self, slices: &mut [usize]) {
//        let mut slices_iter = slices.iter_mut();
//        let mut nhead = self.eptr_load(&self.queue);
//        while let Some((slice, head)) = slices_iter.next().zip(nhead) {
//            let mut count = 0;
//            let mut nsibling = Some(head);
//            while let Some(sibling) = nsibling {
//                count += 1;
//                nsibling = self.eptr_load(&sibling.sibling);
//            }
//
//            *slice = count;
//            nhead = self.eptr_load(&head.next);
//        }
//    }
}



