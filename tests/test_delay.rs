
use equeue::Equeue;

use std::mem::transmute;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::sync::Mutex;
use std::ops::Deref;

#[test]
fn test_delay() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let count = AtomicU32::new(0);
    for i in 0..10 {
        q.call_in(i*100, || {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap();
    }

    q.dispatch(50);
    for i in 0..10 {
        assert_eq!(count.load(Ordering::SeqCst), i+1);
        q.dispatch(100);
    }
    q.dispatch(100);

    assert_eq!(count.load(Ordering::SeqCst), 10);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_delay_many() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let count = AtomicU32::new(0);
    for i in 0..10 {
        for _ in 0..100 {
            q.call_in(i*100, || {
                count.fetch_add(1, Ordering::SeqCst);
            }).unwrap();
        }
    }

    q.dispatch(50);
    for i in 0..10 {
        assert_eq!(count.load(Ordering::SeqCst), (i+1)*100);
        q.dispatch(100);
    }
    q.dispatch(100);

    assert_eq!(count.load(Ordering::SeqCst), 10*100);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_delay_interspersed() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let count = AtomicU32::new(0);
    for _ in 0..100 {
        for i in 0..10 {
            q.call_in(i*100, || {
                count.fetch_add(1, Ordering::SeqCst);
            }).unwrap();
        }
    }

    q.dispatch(50);
    for i in 0..10 {
        assert_eq!(count.load(Ordering::SeqCst), (i+1)*100);
        q.dispatch(100);
    }
    q.dispatch(100);

    assert_eq!(count.load(Ordering::SeqCst), 10*100);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_delay_order() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let count = Mutex::new(Vec::new());
    for i in 0..10 {
        for j in 0..10 {
            let count = &count;
            q.call_in(i*100, move || {
                count.lock().unwrap().push(i*10+j)
            }).unwrap();
        }
    }
    q.dispatch(1100);

    assert_eq!(
        count.lock().unwrap().deref(),
        &(0..100).collect::<Vec<_>>()
    );
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_delay_reversed() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let count = Mutex::new(Vec::new());
    for i in (0..10).rev() {
        for j in 0..10 {
            let count = &count;
            q.call_in(i*100, move || {
                count.lock().unwrap().push(i*10+j)
            }).unwrap();
        }
    }
    q.dispatch(1100);

    assert_eq!(
        count.lock().unwrap().deref(),
        &(0..100).collect::<Vec<_>>()
    );
    println!("usage: {:#?}", q.usage());
}
