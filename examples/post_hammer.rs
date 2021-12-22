
use equeue::Equeue;

use std::thread;
use std::alloc::Layout;
use std::mem::transmute;
use std::sync::Arc;
use std::time::Duration;

use rand;
use rand::Rng;


fn main() {
    let mut buffer = vec![0; 1024*1024];
    let q = Arc::new(Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap());

    let mut threads = vec![];

    // one dispatch thread
    {
        let q = q.clone();
        threads.push(thread::spawn(move || {
            loop {
                q.dispatch(0);
                thread::sleep(Duration::from_nanos(1000_000_000));
            }
        }));
    }

    // some busywork, n threads, each a random size
    for _ in 0..100 {
        let q = q.clone();
        threads.push(thread::spawn(move || {
            let mut rng = rand::thread_rng();
            loop {
                // we need to use the raw APIs for dynamic sizes
                let layout = Layout::from_size_align(rng.gen_range(1..8192), 1).unwrap();
                let e = unsafe { q.alloc_raw(layout) };
                if e.is_null() {
                    thread::sleep(Duration::from_nanos(rng.gen_range(0..2000_000_000)));
                    continue;
                }

                fn cb(_data: *mut u8) {
                    // do nothing
                }

                unsafe { q.post_raw(cb, e); }
                thread::sleep(Duration::from_nanos(rng.gen_range(0..2000_000_000)));
            }
        }));
    }

    // and now, in our main thread, lets render something nice looking
    println!();
    let mut bucket_max = 1;
    let mut pending_max = 1;
    loop {
        let usage = q.usage();
        let mut buckets = vec![0; usage.buckets];
        q.bucket_usage(&mut buckets);

        if usage.pending > pending_max {
            pending_max = usage.pending;
        }

        let mut used_buckets = 0;
        for &bucket in buckets.iter() {
            if bucket > bucket_max {
                bucket_max = bucket
            }

            if bucket != 0 {
                used_buckets += 1;
            }
        }

        // render this thing
        let width = 48;

        print!("\x1b[K  q ");
        for _ in 0 .. (width-2)*usage.pending / pending_max {
            print!("'");
        }
        for _ in (width-2)*usage.pending / pending_max .. width-2 {
            print!(" ");
        }
        println!("  pending: {}", usage.pending);

        let print_row = |row: f64| {
            for (_npw2, &bucket) in buckets.iter().enumerate() {
                let bucket_dots = 6.0*((bucket as f64) / (bucket_max as f64));
                for _ in 0 .. 2 {
                    if bucket_dots > 2.0*row+1.0 {
                        print!(":");
                    } else if bucket_dots > 2.0*row {
                        print!(".");
                    } else {
                        print!(" ");
                    }
                }
            }

            for _ in 2*buckets.len() .. width-2 {
                print!(" ");
            }
        };

        print!("\x1b[K    "); print_row(2.0); println!("  slab_total: {}", usage.slab_total);
        print!("\x1b[K    "); print_row(1.0); println!("  slab_free: {}", usage.slab_free);
        print!("\x1b[K  f "); print_row(0.0); println!("  slab_fragmented: {}", usage.slab_fragmented);
        
        print!("\x1b[K  [");
        let used_bars = ((((usage.slab_total - usage.slab_free - usage.slab_fragmented) as f64)
            / (usage.slab_total as f64))
            * ((width-2) as f64))
            as usize;
        let fragmented_bars = ((((usage.slab_total - usage.slab_free) as f64)
            / (usage.slab_total as f64))
            * ((width-2) as f64))
            as usize;
        for _ in 0 .. used_bars {
            print!("|");
        }
        for _ in 0 .. fragmented_bars - used_bars {
            print!(":");
        }
        for _ in 0 .. width-2 - fragmented_bars {
            print!(" ");
        }
        print!("]");
        println!("  buckets: {}/{}", used_buckets, usage.buckets);

        thread::sleep(Duration::from_millis(10));

        print!("\x1b[5F");
    }
}
