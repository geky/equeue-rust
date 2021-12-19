
use equeue::Equeue;

use std::mem::transmute;
use std::alloc::Layout;
use std::thread;
use std::sync::Mutex;
use std::sync::Arc;
use std::collections::HashSet;

#[test]
fn test_alloc_unique() {
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

    println!("usage: {:?}", q.usage());
}

#[test]
fn test_alloc_multiple() {
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

    println!("usage: {:?}", q.usage());
}

#[test]
fn test_alloc_many() {
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

    println!("usage: {:?}", q.usage());
}
