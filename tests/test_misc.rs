
use equeue::Equeue;
use equeue::Delta;
use equeue::Dispatch;
use equeue::Config;
use equeue::Buffer;
use equeue::Clock;
use equeue::Sema;
use equeue::sys::SysClock;
use equeue::sys::utick;

use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::time::Duration;
use std::mem::transmute;

use async_io::block_on;


#[test]
fn test_misc_buffer() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    );

    let count = AtomicU32::new(0);
    for _ in 0..1000 {
        q.call(|| {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap();
    }
    q.dispatch(Some(Duration::from_millis(0)));

    assert_eq!(count.load(Ordering::SeqCst), 1000);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_misc_config() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_config(Config {
        clock: SysClock::new(),
        precision: None,
        buffer: Buffer::Static(
            unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
        )
    });

    let count = AtomicU32::new(0);
    for _ in 0..1000 {
        q.call(|| {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap();
    }
    q.dispatch(Some(Duration::from_millis(0)));

    assert_eq!(count.load(Ordering::SeqCst), 1000);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_misc_custom_clock() {
    // We're only going to do post on this, so we don't need it to be exhaustive
    #[derive(Debug)]
    struct MyClock();

    impl Clock for MyClock {
        fn now(&self) -> utick {
            0
        }
    }

    impl Sema for MyClock {
        fn signal(&self) {}
        fn wait(&self, _: Option<Delta>) {
            unreachable!();
        }
    }

    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_config(Config {
        clock: MyClock(),
        precision: None,
        buffer: Buffer::Static(
            unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
        )
    });

    let count = AtomicU32::new(0);
    for _ in 0..1000 {
        q.call(|| {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap();
    }
    q.dispatch(Some(Delta::zero()));

    assert_eq!(count.load(Ordering::SeqCst), 1000);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_misc_break() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    for i in 0..10 {
        for _ in 0..10 {
            q.call_in(Duration::from_millis(i*100), || {
                count.fetch_add(1, Ordering::SeqCst);
            }).unwrap();
        }
    }
    q.call_in(Duration::from_millis(250), || {
        q.break_();
    }).unwrap();
    q.call_in(Duration::from_millis(450), || {
        q.break_();
    }).unwrap();

    assert_eq!(
        q.dispatch(None::<Delta>),
        Dispatch::Break,
    );
    assert_eq!(count.load(Ordering::SeqCst), 30);

    assert_eq!(
        q.dispatch(None::<Delta>),
        Dispatch::Break,
    );
    assert_eq!(count.load(Ordering::SeqCst), 50);

    assert_eq!(
        q.dispatch(Some(Duration::from_millis(1100))),
        Dispatch::Timeout,
    );
    assert_eq!(count.load(Ordering::SeqCst), 100);

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_misc_break_busy() {
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
        q.dispatch(None::<Delta>),
        Dispatch::Break,
    );
    assert_eq!(count.load(Ordering::SeqCst), 10);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_misc_async_dispatch() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    for i in 0..10 {
        q.call_in(Duration::from_millis(i*100), || {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap();
    }

    block_on(async {
        q.dispatch_async(Some(Duration::from_millis(50))).await;
        for i in 0..10 {
            assert_eq!(count.load(Ordering::SeqCst), i+1);
            q.dispatch_async(Some(Duration::from_millis(100))).await;
        }
        q.dispatch_async(Some(Duration::from_millis(100))).await;
    });

    assert_eq!(count.load(Ordering::SeqCst), 10);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_misc_nested_async_dispatch() {
    let q1 = Equeue::with_size(1024*1024);
    let q2 = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    for i in 0..10 {
        q2.call_in(Duration::from_millis(i*100), || {
            count.fetch_add(1, Ordering::SeqCst);
        }).unwrap();
    }

    q1.run(async {
        q2.dispatch_async(Some(Duration::from_millis(50))).await;
        for i in 0..10 {
            assert_eq!(count.load(Ordering::SeqCst), i+1);
            q2.dispatch_async(Some(Duration::from_millis(100))).await;
        }
        q2.dispatch_async(Some(Duration::from_millis(100))).await;
    }).unwrap();

    q1.dispatch(Some(Duration::from_millis(50)));
    for i in 0..10 {
        assert_eq!(count.load(Ordering::SeqCst), i+1);
        q1.dispatch(Some(Duration::from_millis(100)));
    }
    q1.dispatch(Some(Duration::from_millis(100)));

    assert_eq!(count.load(Ordering::SeqCst), 10);
    println!("usage: {:#?}", q1.usage());
    println!("usage: {:#?}", q2.usage());
}

#[test]
fn test_misc_handles() {
    let q = Equeue::with_size(1024*1024);

    let count = AtomicU32::new(0);
    let mut handles = vec![];
    for _ in 0..100 {
        handles.push(
            q.alloc(|| {
                count.fetch_add(1, Ordering::SeqCst);
            }).unwrap()
                .period(Some(Duration::from_millis(0)))
                .into_handle()
        );
    }

    // cancel half, pend the other half
    for (i, handle) in handles.iter().enumerate() {
        if i % 2 == 0 {
            assert!(handle.pend());
        } else {
            assert!(handle.cancel());
        }
    }

    assert_eq!(count.load(Ordering::SeqCst), 0);
    q.dispatch(Some(Duration::from_millis(0)));
    assert_eq!(count.load(Ordering::SeqCst), 50);

    // no drop another half
    handles.truncate(50);

    assert_eq!(count.load(Ordering::SeqCst), 50);
    q.dispatch(Some(Duration::from_millis(0)));
    assert_eq!(count.load(Ordering::SeqCst), 75);

    // and release the rest
    drop(handles);

    assert_eq!(count.load(Ordering::SeqCst), 75);
    q.dispatch(Some(Duration::from_millis(0)));
    assert_eq!(count.load(Ordering::SeqCst), 75);

    println!("usage: {:#?}", q.usage());
}
