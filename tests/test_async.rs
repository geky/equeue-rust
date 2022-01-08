
use equeue::Equeue;

use std::mem::transmute;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::time::Duration;

use async_std::task::yield_now;
use async_std::task::sleep;

#[test]
fn test_async() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let count = AtomicU32::new(0);
    q.run(async {
        count.fetch_add(1, Ordering::SeqCst);
    }).unwrap();
    q.dispatch(0);

    assert_eq!(count.load(Ordering::SeqCst), 1);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_async_several() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let count = AtomicU32::new(0);
    q.run(async {
        for _ in 0..100 {
            count.fetch_add(1, Ordering::SeqCst);
            yield_now().await;
        }
    }).unwrap();

    for i in 0..100 {
        q.dispatch(0);
        assert_eq!(count.load(Ordering::SeqCst), i+1);
    }

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_async_multiple() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let count = AtomicU32::new(0);
    for _ in 0..100 {
        q.run(async {
            for _ in 0..100 {
                count.fetch_add(1, Ordering::SeqCst);
                yield_now().await;
            }
        }).unwrap();
    }

    for i in 0..100 {
        q.dispatch(0);
        assert_eq!(count.load(Ordering::SeqCst), (i+1)*100);
    }

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_async_multiple_async_std_sleep() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let count = AtomicU32::new(0);
    for _ in 0..100 {
        q.run(async {
            for _ in 0..10 {
                count.fetch_add(1, Ordering::SeqCst);
                sleep(Duration::from_millis(100)).await;
            }
        }).unwrap();
    }

    for i in 0..10 {
        q.dispatch(50);
        assert_eq!(count.load(Ordering::SeqCst), (i+1)*100);
        q.dispatch(50);
    }

    // a bit of extra time so all futures are collected
    q.dispatch(100);
    assert_eq!(count.load(Ordering::SeqCst), 10*100);
    println!("usage: {:#?}", q.usage());
}
