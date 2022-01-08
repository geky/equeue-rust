
use equeue::Equeue;
use equeue::Error;

use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

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
