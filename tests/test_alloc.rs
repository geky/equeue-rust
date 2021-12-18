
use equeue::Equeue;

use std::mem::transmute;
use std::alloc::Layout;
use std::ptr;

#[test]
fn test_alloc() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let layout = Layout::from_size_align(100, 1).unwrap();
    let e = unsafe { q.alloc_raw(layout) };
    assert!(!e.is_null());
    unsafe { q.dealloc_raw(e, layout) };

    println!("usage: {:?}", q.usage());
}

#[test]
fn test_alloc_multiple() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let layout = Layout::from_size_align(100, 1).unwrap();
    let mut es = vec![];
    for _ in 0..100 {
        let e = unsafe { q.alloc_raw(layout) };
        assert!(!e.is_null());
        es.push(e);
    }

    for i in 0..100 {
        unsafe { q.dealloc_raw(es[i], layout) };
    }

    println!("usage: {:?}", q.usage());
}

#[test]
fn test_alloc_many() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let mut es = vec![];
    for i in 0..100 {
        let layout = Layout::from_size_align(i*10, 1).unwrap();
        let e = unsafe { q.alloc_raw(layout) };
        assert!(!e.is_null());
        es.push(e);
    }

    for i in 0..100 {
        let layout = Layout::from_size_align(i*10, 1).unwrap();
        unsafe { q.dealloc_raw(es[i], layout) };
    }

    println!("usage: {:?}", q.usage());
}

#[test]
fn test_alloc_repeatedly() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    for _ in 0..100 {
        let layout = Layout::from_size_align(100, 1).unwrap();
        let e = unsafe { q.alloc_raw(layout) };
        assert!(!e.is_null());
        unsafe { q.dealloc_raw(e, layout) };
    }

    println!("usage: {:?}", q.usage());
}

#[test]
fn test_alloc_multiple_repeatedly() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    for _ in 0..100 {
        let layout = Layout::from_size_align(100, 1).unwrap();
        let mut es = vec![];
        for _ in 0..100 {
            let e = unsafe { q.alloc_raw(layout) };
            assert!(!e.is_null());
            es.push(e);
        }

        for i in 0..100 {
            unsafe { q.dealloc_raw(es[i], layout) };
        }
    }

    println!("usage: {:?}", q.usage());
}

#[test]
fn test_alloc_many_repeatedly() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    for _ in 0..100 {
        let mut es = vec![];
        for i in 0..100 {
            let layout = Layout::from_size_align(i*10, 1).unwrap();
            let e = unsafe { q.alloc_raw(layout) };
            assert!(!e.is_null());
            es.push(e);
        }

        for i in 0..100 {
            let layout = Layout::from_size_align(i*10, 1).unwrap();
            unsafe { q.dealloc_raw(es[i], layout) };
        }
    }

    println!("usage: {:?}", q.usage());
}

#[test]
fn test_alloc_exhaustion() {
    let mut buffer = vec![0; 1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let layout = Layout::from_size_align(2*1024, 1).unwrap();
    assert_eq!(unsafe { q.alloc_raw(layout) }, ptr::null_mut());

    let layout = Layout::from_size_align(1024, 1).unwrap();
    assert_eq!(unsafe { q.alloc_raw(layout) }, ptr::null_mut());

    let layout = Layout::from_size_align(100, 1).unwrap();
    let e = unsafe { q.alloc_raw(layout) };
    assert!(!e.is_null());
    unsafe { q.dealloc_raw(e, layout) };

    println!("usage: {:?}", q.usage());
}
