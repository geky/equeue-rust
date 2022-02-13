
use equeue::Equeue;

use std::alloc::Layout;
use std::ptr;

#[test]
fn test_alloc() {
    let q = Equeue::with_size(1024*1024);

    let layout = Layout::from_size_align(100, 1).unwrap();
    let e = unsafe { q.alloc_raw(layout, |_|{}, |_|{}) };
    assert!(!e.is_null());
    unsafe { q.dealloc_raw(e) };

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_alloc_multiple() {
    let q = Equeue::with_size(1024*1024);

    let layout = Layout::from_size_align(100, 1).unwrap();
    let mut es = vec![];
    for _ in 0..100 {
        let e = unsafe { q.alloc_raw(layout, |_|{}, |_|{}) };
        assert!(!e.is_null());
        es.push(e);
    }

    for i in 0..100 {
        unsafe { q.dealloc_raw(es[i]) };
    }

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_alloc_many() {
    let q = Equeue::with_size(1024*1024);

    let mut es = vec![];
    for i in 0..100 {
        let layout = Layout::from_size_align(i*10, 1).unwrap();
        let e = unsafe { q.alloc_raw(layout, |_|{}, |_|{}) };
        assert!(!e.is_null());
        es.push(e);
    }

    for i in 0..100 {
        unsafe { q.dealloc_raw(es[i]) };
    }

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_alloc_repeatedly() {
    let q = Equeue::with_size(1024*1024);

    for _ in 0..100 {
        let layout = Layout::from_size_align(100, 1).unwrap();
        let e = unsafe { q.alloc_raw(layout, |_|{}, |_|{}) };
        assert!(!e.is_null());
        unsafe { q.dealloc_raw(e) };
    }

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_alloc_multiple_repeatedly() {
    let q = Equeue::with_size(1024*1024);

    for _ in 0..100 {
        let layout = Layout::from_size_align(100, 1).unwrap();
        let mut es = vec![];
        for _ in 0..100 {
            let e = unsafe { q.alloc_raw(layout, |_|{}, |_|{}) };
            assert!(!e.is_null());
            es.push(e);
        }

        for i in 0..100 {
            unsafe { q.dealloc_raw(es[i]) };
        }
    }

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_alloc_many_repeatedly() {
    let q = Equeue::with_size(1024*1024);

    for _ in 0..100 {
        let mut es = vec![];
        for i in 0..100 {
            let layout = Layout::from_size_align(i*10, 1).unwrap();
            let e = unsafe { q.alloc_raw(layout, |_|{}, |_|{}) };
            assert!(!e.is_null());
            es.push(e);
        }

        for i in 0..100 {
            unsafe { q.dealloc_raw(es[i]) };
        }
    }

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_alloc_exhaustion() {
    let q = Equeue::with_size(1024);

    let layout = Layout::from_size_align(2*1024, 1).unwrap();
    assert_eq!(unsafe { q.alloc_raw(layout, |_|{}, |_|{}) }, ptr::null_mut());

    let layout = Layout::from_size_align(1024, 1).unwrap();
    assert_eq!(unsafe { q.alloc_raw(layout, |_|{}, |_|{}) }, ptr::null_mut());

    let layout = Layout::from_size_align(100, 1).unwrap();
    let e = unsafe { q.alloc_raw(layout, |_|{}, |_|{}) };
    assert!(!e.is_null());
    unsafe { q.dealloc_raw(e) };

    println!("usage: {:#?}", q.usage());
}
