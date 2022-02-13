
use equeue::Equeue;
use equeue::Delta;
use equeue::sys::SysClock;

use loom::thread;

use std::collections::HashSet;
use std::alloc::Layout;

const fn parse_const_usize(s: &str) -> usize {
    let mut v = 0;
    let s = s.as_bytes();

    let mut i = 0;
    while i < s.len() {
        if s[i] >= b'0' && s[i] <= b'9' {
            v = v*10 + (s[i] - b'0') as usize;
        } else {
            panic!("invalid compile-time usize");
        }
        i += 1;
    }

    v
}

// Number of threads to test with, actual usage depends on tests
//
// Even at 2 threads this already takes ~12 minutes
//
const LOOM_THREADS: usize = {
    match option_env!("EQUEUE_LOOM_THREADS") {
        Some(threads) => parse_const_usize(threads),
        None          => 2,
    }
};

// The logic in equeue ends up with a large number of branches, so we end up
// triggering infinite-loop protection when we shouldn't. This is the highest
// we can set LOOM_MAX_BRANCHES due to an internal u16 loom uses, but it's
// enough to pass our tests.
//
// Note this is still overridable.
//
const LOOM_MAX_BRANCHES: usize = {
    match option_env!("LOOM_MAX_BRANCHES") {
        Some(max_branches) => parse_const_usize(max_branches),
        None               => 65535,
    }
};

fn loom_model() -> loom::model::Builder {
    let mut cfg = loom::model::Builder::new();
    cfg.max_branches = LOOM_MAX_BRANCHES;
    cfg
}


#[test]
fn test_loom_alloc_unique() {
    loom_model().check(|| {
        // note these use the std sync types because we don't
        // need to test if Arc or Mutex work
        let q = std::sync::Arc::new(Equeue::with_size(1024*1024));
        let set = std::sync::Arc::new(std::sync::Mutex::new(HashSet::new()));

        let mut threads = vec![];
        for _ in 0..LOOM_THREADS {
            let q = q.clone();
            let set = set.clone();
            threads.push(thread::spawn(move || {
                let mut es = vec![];
                let layout = Layout::from_size_align(4, 1).unwrap();
                for _ in 0..10 {
                    let e = unsafe { q.alloc_raw(layout, |_|{}, |_|{}) };
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

        assert_eq!(set.lock().unwrap().len(), LOOM_THREADS*10);
    })
}

#[test]
fn test_loom_post() {
    loom_model().check(|| {
        // note these use the std sync types because we don't
        // need to test if Arc or Mutex work
        let q = std::sync::Arc::new(Equeue::with_size(1024*1024));
        let counts = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));

        let mut threads = vec![];
        for j in 0..LOOM_THREADS {
            counts.lock().unwrap().push(vec![]);

            let q = q.clone();
            let counts = counts.clone();
            threads.push(thread::spawn(move || {
                for i in 0..3 {
                    let counts = counts.clone();
                    q.call(move || {
                        counts.lock().unwrap()[j].push(i);
                    }).unwrap();
                }
            }));
        }

        for thread in threads.into_iter() {
            thread.join().unwrap();
        }

        // catch any lingering events
        q.dispatch_ready();

        for j in 0..LOOM_THREADS {
            assert_eq!(
                &counts.lock().unwrap()[j],
                &(0..3).collect::<Vec<_>>()
            );
        }
    })
}

#[test]
fn test_loom_post_delay() {
    loom_model().check(|| {
        // note these use the std sync types because we don't
        // need to test if Arc or Mutex work
        let q = std::sync::Arc::new(Equeue::with_size(1024*1024));
        let counts = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));

        let mut threads = vec![];
        for j in 0..LOOM_THREADS {
            counts.lock().unwrap().push(vec![]);

            let q = q.clone();
            let counts = counts.clone();
            threads.push(thread::spawn(move || {
                for i in 0..3 {
                    let counts = counts.clone();
                    q.call_in(Delta::new(i*1000).unwrap(), move || {
                        counts.lock().unwrap()[j].push(i);
                    }).unwrap();
                }
            }));
        }

        // increment time
        let clock_thread = thread::spawn(move || {
            for i in 0..3 {
                SysClock::set_now((i+1)*1000);
            }
        });

        for thread in threads.into_iter() {
            thread.join().unwrap();
        }
        clock_thread.join().unwrap();

        // catch any lingering events
        SysClock::set_now((20+1)*1000);
        q.dispatch_ready();

        for j in 0..LOOM_THREADS {
            assert_eq!(
                &counts.lock().unwrap()[j],
                &(0..3).collect::<Vec<_>>()
            );
        }
    })
}

#[test]
fn test_loom_post_dispatch() {
    loom_model().check(|| {
        // note these use the std sync types because we don't
        // need to test if Arc or Mutex work
        let q = std::sync::Arc::new(Equeue::with_size(1024*1024));
        let counts = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));

        let mut threads = vec![];
        for j in 0..LOOM_THREADS-1 {
            counts.lock().unwrap().push(vec![]);

            let q = q.clone();
            let counts = counts.clone();
            threads.push(thread::spawn(move || {
                for i in 0..3 {
                    let counts = counts.clone();
                    q.call(move || {
                        counts.lock().unwrap()[j].push(i);
                    }).unwrap();
                }
            }));
        }

        // we only need one dispatch to let loom test all permutations
        thread::yield_now();
        q.dispatch_ready();

        for thread in threads.into_iter() {
            thread.join().unwrap();
        }

        // catch any lingering events
        q.dispatch_ready();

        for j in 0..LOOM_THREADS-1 {
            assert_eq!(
                &counts.lock().unwrap()[j],
                &(0..3).collect::<Vec<_>>()
            );
        }
    })
}

#[test]
fn test_loom_post_delay_dispatch() {
    loom_model().check(|| {
        // note these use the std sync types because we don't
        // need to test if Arc or Mutex work
        let q = std::sync::Arc::new(Equeue::with_size(1024*1024));
        let counts = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));

        let mut threads = vec![];
        for j in 0..LOOM_THREADS-1 {
            counts.lock().unwrap().push(vec![]);

            let q = q.clone();
            let counts = counts.clone();
            threads.push(thread::spawn(move || {
                for i in 0..3 {
                    let counts = counts.clone();
                    q.call_in(Delta::new(i*1000).unwrap(), move || {
                        counts.lock().unwrap()[j].push(i);
                    }).unwrap();
                }
            }));
        }

        // increment time
        let clock_thread = thread::spawn(move || {
            for i in 0..3 {
                SysClock::set_now((i+1)*1000);
            }
        });

        // we only need one dispatch to let loom test all permutations
        thread::yield_now();
        q.dispatch_ready();

        for thread in threads.into_iter() {
            thread.join().unwrap();
        }
        clock_thread.join().unwrap();

        // catch any lingering events
        SysClock::set_now((20+1)*1000);
        q.dispatch_ready();

        for j in 0..LOOM_THREADS-1 {
            assert_eq!(
                &counts.lock().unwrap()[j],
                &(0..3).collect::<Vec<_>>()
            );
        }
    })
}

#[test]
fn test_loom_post_cancel() {
    loom_model().check(|| {
        // note these use the std sync types because we don't
        // need to test if Arc or Mutex work
        let q = std::sync::Arc::new(Equeue::with_size(1024*1024));
        let counts = std::sync::Arc::new(loom::sync::Mutex::new(Vec::new()));

        let mut threads = vec![];
        for j in 0..LOOM_THREADS {
            counts.lock().unwrap().push(vec![]);

            let q = q.clone();
            let counts = counts.clone();
            threads.push(thread::spawn(move || {
                let mut ids = vec![];
                for i in 0..3 {
                    let counts = counts.clone();
                    ids.push(
                        q.call(move || {
                            counts.lock().unwrap()[j].push(i);
                        }).unwrap()
                    );
                }

                for i in 0..3 {
                    // cancel roughly half
                    if (j+i) % 2 == 0 {
                        q.cancel(ids[i]);
                    }
                }
            }));
        }

        for thread in threads.into_iter() {
            thread.join().unwrap();
        }

        // catch any lingering events
        q.dispatch_ready();

        for j in 0..LOOM_THREADS {
            assert_eq!(
                &counts.lock().unwrap()[j],
                &(0..3).filter(|i| (j+i) % 2 != 0).collect::<Vec<_>>(),
            );
        }
    })
}

#[test]
fn test_loom_post_delay_cancel() {
    loom_model().check(|| {
        // note these use the std sync types because we don't
        // need to test if Arc or Mutex work
        let q = std::sync::Arc::new(Equeue::with_size(1024*1024));
        let counts = std::sync::Arc::new(loom::sync::Mutex::new(Vec::new()));

        let mut threads = vec![];
        for j in 0..LOOM_THREADS {
            counts.lock().unwrap().push(vec![]);

            let q = q.clone();
            let counts = counts.clone();
            threads.push(thread::spawn(move || {
                let mut ids = vec![];
                for i in 0..3 {
                    let counts = counts.clone();
                    ids.push(
                        q.call_in(Delta::new(i*1000).unwrap(), move || {
                            counts.lock().unwrap()[j].push(i);
                        }).unwrap()
                    );
                }

                for i in 0..3 {
                    // cancel roughly half
                    if (j+i as usize) % 2 == 0 {
                        q.cancel(ids[i]);
                    }
                }
            }));
        }

        for thread in threads.into_iter() {
            thread.join().unwrap();
        }

        // catch any lingering events
        SysClock::set_now((10+1)*1000);
        q.dispatch_ready();

        for j in 0..LOOM_THREADS {
            assert_eq!(
                &counts.lock().unwrap()[j],
                &(0..3).filter(|&i| (j+i as usize) % 2 != 0).collect::<Vec<_>>(),
            );
        }
    })
}

