
use equeue::Equeue;

use std::sync::Mutex;
use std::ops::Deref;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::time::Duration;

#[test]
fn test_post() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    q.call(|| {
        count.fetch_add(1, Ordering::SeqCst);
    }).unwrap();
    q.dispatch_for(Duration::from_millis(0));

    assert_eq!(count.load(Ordering::SeqCst), 1);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_post_many() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    for _ in 0..1000 {
        q.call(|| {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap();
    }
    q.dispatch_for(Duration::from_millis(0));

    assert_eq!(count.load(Ordering::SeqCst), 1000);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_post_order() {
    let q = Equeue::with_size(1024*1024);

    let count = Mutex::new(Vec::new());
    for i in 0..1000 {
        let count = &count;
        q.call(move || {
            count.lock().unwrap().push(i)
        }).unwrap();
    }
    q.dispatch_for(Duration::from_millis(0));

    assert_eq!(
        count.lock().unwrap().deref(),
        &(0..1000).collect::<Vec<_>>()
    );
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_post_recursive() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    fn inc(q: &Equeue, count: &AtomicU32) {
        if count.fetch_add(1, Ordering::SeqCst) <= 1000 {
            q.call(|| inc(q, count)).unwrap();
        }
    }
    q.call(|| inc(&q, &count)).unwrap();

    for i in 0..1000 {
        q.dispatch_for(Duration::from_millis(0));
        assert_eq!(count.load(Ordering::SeqCst), i+1);
    }

    println!("usage: {:#?}", q.usage());
}
