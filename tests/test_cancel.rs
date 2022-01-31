
use equeue::Equeue;

use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::time::Duration;

#[test]
fn test_cancel() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    let id = q.call(|| {
        count.fetch_add(1, Ordering::SeqCst);
    }).unwrap();

    assert_eq!(q.cancel(id), true);
    q.dispatch_ready();

    assert_eq!(count.load(Ordering::SeqCst), 0);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_cancel_dont() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    let id = q.call(|| {
        count.fetch_add(1, Ordering::SeqCst);
    }).unwrap();
    q.dispatch_ready();

    assert_eq!(q.cancel(id), false);

    assert_eq!(count.load(Ordering::SeqCst), 1);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_cancel_many() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    let mut ids = vec![];
    for _ in 0..1000 {
        ids.push(q.call(|| {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap());
    }

    for id in ids {
        assert_eq!(q.cancel(id), true);
    }
    q.dispatch_ready();

    assert_eq!(count.load(Ordering::SeqCst), 0);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_cancel_many_reversed() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    let mut ids = vec![];
    for _ in 0..1000 {
        ids.push(q.call(|| {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap());
    }

    for &id in ids.iter().rev() {
        assert_eq!(q.cancel(id), true);
    }
    q.dispatch_ready();

    assert_eq!(count.load(Ordering::SeqCst), 0);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_cancel_many_delay() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    let mut ids = vec![];
    for i in 0..10 {
        for _ in 0..1000 {
            ids.push(q.call_in(Duration::from_millis(i*100), || {
                count.fetch_add(1, Ordering::SeqCst);
            }).unwrap());
        }
    }

    for id in ids {
        assert_eq!(q.cancel(id), true);
    }
    q.dispatch_for(Duration::from_millis(1100));

    assert_eq!(count.load(Ordering::SeqCst), 0);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_cancel_many_delay_reversed() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    let mut ids = vec![];
    for i in 0..10 {
        for _ in 0..1000 {
            ids.push(q.call_in(Duration::from_millis(i*100), || {
                count.fetch_add(1, Ordering::SeqCst);
            }).unwrap());
        }
    }

    for &id in ids.iter().rev() {
        assert_eq!(q.cancel(id), true);
    }
    q.dispatch_for(Duration::from_millis(1100));

    assert_eq!(count.load(Ordering::SeqCst), 0);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_cancel_many_periodic() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    let mut ids = vec![];
    for i in 0..10 {
        for _ in 0..1000 {
            ids.push(q.call_every(Duration::from_millis(i*100), || {
                count.fetch_add(1, Ordering::SeqCst);
            }).unwrap());
        }
    }

    q.dispatch_for(Duration::from_millis(1100));
    let before = count.load(Ordering::SeqCst);

    for id in ids {
        assert_eq!(q.cancel(id), true);
    }

    q.dispatch_for(Duration::from_millis(1100));
    let after = count.load(Ordering::SeqCst);

    assert_eq!(before, after);
    println!("usage: {:#?}", q.usage());
}
