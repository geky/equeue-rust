
use equeue::Equeue;
use equeue::Error;

use std::mem::transmute;
use std::alloc::Layout;
use std::thread;
use std::sync::Mutex;
use std::sync::Arc;
use std::collections::HashSet;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

#[test]
fn test_race_alloc_unique() {
    let mut buffer = vec![0; 1024*1024];
    let q = Arc::new(Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap());

    let set = Arc::new(Mutex::new(HashSet::new()));

    let mut threads = vec![];
    for _ in 0..100 {
        let q = q.clone();
        let set = set.clone();
        threads.push(thread::spawn(move || {
            let mut es = vec![];
            let layout = Layout::from_size_align(10, 1).unwrap();
            for _ in 0..100 {
                let e = unsafe { q.alloc_raw(layout) };
                assert!(!e.is_null());
                es.push(e);
            }

            let mut set = set.lock().unwrap();
            for e in es {
                set.insert(e as usize);
            }
        }));
    }

    for thread in threads.into_iter() {
        thread.join().unwrap();
    }

    assert_eq!(set.lock().unwrap().len(), 100*100);

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_race_alloc_multiple() {
    let mut buffer = vec![0; 1024*1024];
    let q = Arc::new(Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap());

    let mut threads = vec![];
    for _ in 0..100 {
        let q = q.clone();
        threads.push(thread::spawn(move || {
            let layout = Layout::from_size_align(10, 1).unwrap();
            for _ in 0..1000 {
                let e = unsafe { q.alloc_raw(layout) };
                assert!(!e.is_null());
                unsafe { q.dealloc_raw(e, layout) };
            }
        }));
    }

    for thread in threads.into_iter() {
        thread.join().unwrap();
    }

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_race_alloc_many() {
    let mut buffer = vec![0; 1024*1024];
    let q = Arc::new(Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap());

    let mut threads = vec![];
    for _ in 0..100 {
        let q = q.clone();
        threads.push(thread::spawn(move || {
            for i in 0..1000 {
                let layout = Layout::from_size_align(10*i, 1).unwrap();
                let e = unsafe { q.alloc_raw(layout) };
                assert!(!e.is_null());
                unsafe { q.dealloc_raw(e, layout) };
            }
        }));
    }

    for thread in threads.into_iter() {
        thread.join().unwrap();
    }

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_race_post() {
    let mut buffer = vec![0; 1024*1024];
    let q = Arc::new(Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap());

    let count = Arc::new(AtomicU32::new(0));
    let done = Arc::new(AtomicBool::new(false));

    let dispatch_thread = {
        let q = q.clone();
        let done = done.clone();
        thread::spawn(move || {
            while !done.load(Ordering::SeqCst) {
                q.dispatch(0);
            }

            // make sure we catch any lingering events
            q.dispatch(0);
        })
    };

    let mut threads = vec![];
    for _ in 0..100 {
        let q = q.clone();
        let count = count.clone();
        threads.push(thread::spawn(move || {
            for _ in 0..1000 {
                let count = count.clone();
                let cb = move || {
                    count.fetch_add(1, Ordering::SeqCst);
                };
                loop {
                    match q.call(cb.clone()) {
                        Ok(_) => break,
                        Err(Error::NoMem) => { thread::yield_now(); continue },
                    }
                }
            }
        }));
    }

    for thread in threads.into_iter() {
        thread.join().unwrap();
    }
    done.fetch_or(true, Ordering::SeqCst);
    dispatch_thread.join().unwrap();

    assert_eq!(count.load(Ordering::SeqCst), 100*1000);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_race_post_order() {
    let mut buffer = vec![0; 1024*1024];
    let q = Arc::new(Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap());

    let counts = Arc::new(Mutex::new(Vec::new()));
    let done = Arc::new(AtomicBool::new(false));

    let dispatch_thread = {
        let q = q.clone();
        let done = done.clone();
        thread::spawn(move || {
            while !done.load(Ordering::SeqCst) {
                q.dispatch(0);
            }

            // make sure we catch any lingering events
            q.dispatch(0);
        })
    };

    let mut threads = vec![];
    for j in 0..100 {
        counts.lock().unwrap().push(vec![]);

        let q = q.clone();
        let counts = counts.clone();
        threads.push(thread::spawn(move || {
            for i in 0..1000 {
                let counts = counts.clone();
                let cb = move || {
                    counts.lock().unwrap()[j].push(i);
                };
                loop {
                    match q.call(cb.clone()) {
                        Ok(_) => break,
                        Err(Error::NoMem) => { thread::yield_now(); continue },
                    }
                }
            }
        }));
    }

    for thread in threads.into_iter() {
        thread.join().unwrap();
    }
    done.fetch_or(true, Ordering::SeqCst);
    dispatch_thread.join().unwrap();

    for j in 0..100 {
        assert_eq!(
            &counts.lock().unwrap()[j],
            &(0..1000).collect::<Vec<_>>()
        );
    }
    println!("usage: {:#?}", q.usage());
}
