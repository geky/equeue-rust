
use equeue::Equeue;
use equeue::Error;

use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

use async_io::block_on;

#[test]
fn test_break() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    for i in 0..10 {
        for _ in 0..10 {
            q.call_in(i*100, || {
                count.fetch_add(1, Ordering::SeqCst);
            }).unwrap();
        }
    }
    q.call_in(250, || {
        q.break_();
    }).unwrap();
    q.call_in(450, || {
        q.break_();
    }).unwrap();

    assert_eq!(
        q.dispatch(1100),
        Error::Break,
    );
    assert_eq!(count.load(Ordering::SeqCst), 30);

    assert_eq!(
        q.dispatch(1100),
        Error::Break,
    );
    assert_eq!(count.load(Ordering::SeqCst), 50);

    assert_eq!(
        q.dispatch(1100),
        Error::Timeout,
    );
    assert_eq!(count.load(Ordering::SeqCst), 100);

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_break_busy() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    for _ in 0..10 {
        fn inc(q: &Equeue, count: &AtomicU32) {
            if count.fetch_add(1, Ordering::SeqCst) <= 1000 {
                q.call(|| inc(q, count)).unwrap();
            }
        }

        q.call(|| inc(&q, &count)).unwrap();
    }
    q.break_();

    assert_eq!(
        q.dispatch(1100),
        Error::Break,
    );
    assert_eq!(count.load(Ordering::SeqCst), 10);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_async_dispatch() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    for i in 0..10 {
        q.call_in(i*100, || {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap();
    }

    block_on(async {
        q.dispatch_async(50).await;
        for i in 0..10 {
            assert_eq!(count.load(Ordering::SeqCst), i+1);
            q.dispatch_async(100).await;
        }
        q.dispatch_async(100).await;
    });

    assert_eq!(count.load(Ordering::SeqCst), 10);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_nested_async_dispatch() {
    let q1 = Equeue::with_size(1024*1024);
    let q2 = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    for i in 0..10 {
        q2.call_in(i*100, || {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap();
    }

    q1.run(async {
        q2.dispatch_async(50).await;
        for i in 0..10 {
            assert_eq!(count.load(Ordering::SeqCst), i+1);
            q2.dispatch_async(100).await;
        }
        q2.dispatch_async(100).await;
    }).unwrap();

    q1.dispatch(50);
    for i in 0..10 {
        assert_eq!(count.load(Ordering::SeqCst), i+1);
        q1.dispatch(100);
    }
    q1.dispatch(100);

    assert_eq!(count.load(Ordering::SeqCst), 10);
    println!("usage: {:#?}", q1.usage());
    println!("usage: {:#?}", q2.usage());
}
