
use equeue::Equeue;

use std::mem::transmute;
use std::sync::Mutex;
use std::ops::Deref;

#[test]
fn test_post() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let count = Mutex::new(0);
    q.call(|| {
        *count.lock().unwrap() += 1
    }).unwrap();
    q.dispatch(0);

    assert_eq!(*count.lock().unwrap(), 1);
    println!("usage: {:?}", q.usage());
}

#[test]
fn test_post_many() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let count = Mutex::new(0);
    for _ in 0..1000 {
        q.call(|| {
            *count.lock().unwrap() += 1
        }).unwrap();
    }
    q.dispatch(0);

    assert_eq!(*count.lock().unwrap(), 1000);
    println!("usage: {:?}", q.usage());
}

#[test]
fn test_post_order() {
    let mut buffer = vec![0; 1024*1024];
    let q = Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap();

    let count = Mutex::new(Vec::new());
    for i in 0..1000 {
        let count = &count;
        q.call(move || {
            count.lock().unwrap().push(i)
        }).unwrap();
    }
    q.dispatch(0);

    assert_eq!(
        count.lock().unwrap().deref(),
        &(0..1000).collect::<Vec<_>>()
    );
    println!("usage: {:?}", q.usage());
}
