
use equeue::Equeue;
use equeue::Error;
use equeue::PostStatic;
use equeue::Event;

use std::alloc::Layout;
use std::thread;
use std::sync::Mutex;
use std::sync::Arc;
use std::collections::HashSet;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::mem::forget;

#[test]
fn test_race_alloc_unique() {
    let q = Arc::new(Equeue::with_size(1024*1024));

    let set = Arc::new(Mutex::new(HashSet::new()));

    let mut threads = vec![];
    for _ in 0..100 {
        let q = q.clone();
        let set = set.clone();
        threads.push(thread::spawn(move || {
            let mut es = vec![];
            let layout = Layout::from_size_align(4, 1).unwrap();
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
    let q = Arc::new(Equeue::with_size(1024*1024));

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
    let q = Arc::new(Equeue::with_size(1024*1024));

    let mut threads = vec![];
    for i in 0..100 {
        let q = q.clone();
        threads.push(thread::spawn(move || {
            for _ in 0..1000 {
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
    let q = Arc::new(Equeue::with_size(1024*1024));

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
                        Err(err) => panic!("{:?}", err),
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
    let q = Arc::new(Equeue::with_size(1024*1024));

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
                        Err(err) => panic!("{:?}", err),
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

#[test]
fn test_race_delay() {
    let q = Arc::new(Equeue::with_size(1024*1024));

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
            q.dispatch(1100);
        })
    };

    let mut threads = vec![];
    for _ in 0..100 {
        let q = q.clone();
        let count = count.clone();
        threads.push(thread::spawn(move || {
            for _ in 0..100 {
                for i in 0..10 {
                    let count = count.clone();
                    let cb = move || {
                        count.fetch_add(1, Ordering::SeqCst);
                    };
                    loop {
                        match q.call_in(i*100, cb.clone()) {
                            Ok(_) => break,
                            Err(Error::NoMem) => { thread::yield_now(); continue },
                            Err(err) => panic!("{:?}", err),
                        }
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
fn test_race_cancel() {
    let q = Arc::new(Equeue::with_size(1024*1024));

    let count = Arc::new(AtomicU32::new(0));

    let mut threads = vec![];
    let mut ids = vec![];
    for _ in 0..100 {
        for _ in 0..10 {
            for i in 0..10 {
                let count = count.clone();
                let id = q.call_in(i*100, move || {
                    count.fetch_add(1, Ordering::SeqCst);
                }).unwrap();
                ids.push(id);
            }
        }
    }

    for j in 0..100 {
        let q = q.clone();
        let ids = ids[j*100..(j+1)*100].to_owned();
        threads.push(thread::spawn(move || {
            for id in ids {
                assert_eq!(q.cancel(id), true);
            }
        }));
    }

    for thread in threads.into_iter() {
        thread.join().unwrap();
    }

    q.dispatch(1100);
    assert_eq!(count.load(Ordering::SeqCst), 0);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_race_cancel_enqueue() {
    let q = Arc::new(Equeue::with_size(1024*1024));

    let count = Arc::new(AtomicU32::new(0));

    let mut threads = vec![];
    for _ in 0..100 {
        let q = q.clone();
        let count = count.clone();
        threads.push(thread::spawn(move || {
            for _ in 0..100 {
                for i in 0..10 {
                    let count = count.clone();
                    let id = q.call_in(i*100, move || {
                        count.fetch_add(1, Ordering::SeqCst);
                    }).unwrap();
                    assert_eq!(q.cancel(id), true);
                }
            }
        }));
    }

    for thread in threads.into_iter() {
        thread.join().unwrap();
    }

    q.dispatch(1100);
    assert_eq!(count.load(Ordering::SeqCst), 0);
    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_race_cancel_dispatch() {
    let q = Arc::new(Equeue::with_size(1024*1024));

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
            q.dispatch(1100);
        })
    };

    let mut threads = vec![];
    for _ in 0..100 {
        let q = q.clone();
        let count = count.clone();
        threads.push(thread::spawn(move || {
            for _ in 0..100 {
                for i in 0..10 {
                    let count = count.clone();
                    let id = q.call_in(i*100, move || {
                        count.fetch_add(1, Ordering::SeqCst);
                    }).unwrap();
                    q.cancel(id);
                }
            }
        }));
    }

    for thread in threads.into_iter() {
        thread.join().unwrap();
    }

    done.fetch_or(true, Ordering::SeqCst);
    dispatch_thread.join().unwrap();

    // we can't really check any output here, events may or may not
    // be dispatched, instead we just test that nothing explodes

    println!("usage: {:#?}", q.usage());
}

#[test]
fn test_race_cancel_periodic() {
    let q = Arc::new(Equeue::with_size(1024*1024));

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
            let mut ids = vec![];
            for _ in 0..100 {
                let count = count.clone();
                let id = q.call_every(0, move || {
                    count.fetch_add(1, Ordering::SeqCst);
                }).unwrap();
                ids.push(id);
            }

            for id in ids {
                q.cancel(id);
            }
        }));
    }

    for thread in threads.into_iter() {
        thread.join().unwrap();
    }

    done.fetch_or(true, Ordering::SeqCst);
    dispatch_thread.join().unwrap();

    // we can't really check any output here, events may or may not
    // be dispatched, instead we just test that nothing explodes

    println!("usage: {:#?}", q.usage());
}

struct StaticIncrement(Arc<AtomicU32>);

impl PostStatic for StaticIncrement {
    fn post_static(self_: Event<'_, Self>) {
        self_.0.fetch_add(1, Ordering::SeqCst);
        forget(self_); // TODO should we try to avoid this?
    }
}


#[test]
fn test_race_repost() {
    let q = Arc::new(Equeue::with_size(1024*1024));

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

    let id = q.alloc_static(StaticIncrement(count.clone()))
        .unwrap()
        .into_id();

    let mut threads = vec![];
    for _ in 0..100 {
        let q = q.clone();
        threads.push(thread::spawn(move || {
            for _ in 0..100 {
                q.pend(id);
            }
        }));
    }

    for thread in threads.into_iter() {
        thread.join().unwrap();
    }

    std::thread::sleep(std::time::Duration::from_millis(1000));

    done.fetch_or(true, Ordering::SeqCst);
    dispatch_thread.join().unwrap();

    // we can't really check any output here, events may or may not
    // be dispatched, instead we just test that nothing explodes
    assert!(count.load(Ordering::SeqCst) > 0);
    println!("found: {:?}", count.load(Ordering::SeqCst));
    println!("usage: {:#?}", q.usage());
}
