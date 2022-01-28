
use equeue::Equeue;

use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::sync::Mutex;
use std::ops::Deref;
use std::time::Duration;

#[test]
fn test_delay() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    for i in 0..10 {
        q.call_in(Duration::from_millis(i*100), || {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap();
    }

    q.dispatch_for(Duration::from_millis(50));
    for i in 0..10 {
        assert_eq!(count.load(Ordering::SeqCst), i+1);
        q.dispatch_for(Duration::from_millis(100));
    }
    q.dispatch_for(Duration::from_millis(100));

    assert_eq!(count.load(Ordering::SeqCst), 10);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_delay_many() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    for i in 0..10 {
        for _ in 0..100 {
            q.call_in(Duration::from_millis(i*100), || {
                count.fetch_add(1, Ordering::SeqCst);
            }).unwrap();
        }
    }

    q.dispatch_for(Duration::from_millis(50));
    for i in 0..10 {
        assert_eq!(count.load(Ordering::SeqCst), (i+1)*100);
        q.dispatch_for(Duration::from_millis(100));
    }
    q.dispatch_for(Duration::from_millis(100));

    assert_eq!(count.load(Ordering::SeqCst), 10*100);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_delay_interspersed() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    for _ in 0..100 {
        for i in 0..10 {
            q.call_in(Duration::from_millis(i*100), || {
                count.fetch_add(1, Ordering::SeqCst);
            }).unwrap();
        }
    }

    q.dispatch_for(Duration::from_millis(50));
    for i in 0..10 {
        assert_eq!(count.load(Ordering::SeqCst), (i+1)*100);
        q.dispatch_for(Duration::from_millis(100));
    }
    q.dispatch_for(Duration::from_millis(100));

    assert_eq!(count.load(Ordering::SeqCst), 10*100);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_delay_order() {
    let q = Equeue::with_size(1024*1024);

    let count = Mutex::new(Vec::new());
    for i in 0..10 {
        for j in 0..10 {
            let count = &count;
            q.call_in(Duration::from_millis(i*100), move || {
                count.lock().unwrap().push(i*10+j)
            }).unwrap();
        }
    }
    q.dispatch_for(Duration::from_millis(1100));

    assert_eq!(
        count.lock().unwrap().deref(),
        &(0..100).collect::<Vec<_>>()
    );
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_delay_reversed() {
    let q = Equeue::with_size(1024*1024);

    let count = Mutex::new(Vec::new());
    for i in (0..10).rev() {
        for j in 0..10 {
            let count = &count;
            q.call_in(Duration::from_millis(i*100), move || {
                count.lock().unwrap().push(i*10+j)
            }).unwrap();
        }
    }
    q.dispatch_for(Duration::from_millis(1100));

    assert_eq!(
        count.lock().unwrap().deref(),
        &(0..100).collect::<Vec<_>>()
    );
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_periodic() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    for i in 0..10 {
        q.alloc(|| {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap()
            .delay(Duration::from_millis(i*100))
            .period(Some(Duration::from_millis(1000)))
            .post();
    }

    q.dispatch_for(Duration::from_millis(50));
    for i in 0..30 {
        assert_eq!(count.load(Ordering::SeqCst), i+1);
        q.dispatch_for(Duration::from_millis(100));
    }

    println!("usage: {:#?}", q.usage());
}
