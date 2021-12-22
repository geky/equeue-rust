
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

    // some busywork, n threads, each randomly allocating some random size for
    // some random about of time
    let mut threads = vec![];
    for _ in 0..100 {
        let q = q.clone();
        threads.push(thread::spawn(move || {
            let mut rng = rand::thread_rng();
            loop {
                let layout = Layout::from_size_align(rng.gen_range(1..8192), 1).unwrap();
                let e = unsafe { q.alloc_raw(layout) };

                thread::sleep(Duration::from_nanos(rng.gen_range(0..1000000000)));

                unsafe { q.dealloc_raw(e, layout) };
            }
        }));
    }

    // and now, in our main thread, lets render something nice looking
    println!();
    let mut bucket_max = 1;
    loop {
        let usage = q.usage();
        let mut buckets = vec![0; usage.buckets];
        q.bucket_usage(&mut buckets);

        for &bucket in buckets.iter() {
            if bucket > bucket_max {
                bucket_max = bucket
            }
        }

        let width = 48;
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

            for _ in 2*buckets.len() .. width {
                print!(" ");
            }
        };

        print!("  "); print_row(2.0); println!("  slab_total: {}", usage.slab_total);
        print!("  "); print_row(1.0); println!("  slab_free: {}", usage.slab_free);
        print!("  "); print_row(0.0); println!("  slab_fragmented: {}", usage.slab_fragmented);
        
        print!("  [");
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
        println!("  buckets: {}", usage.buckets);

        thread::sleep(Duration::from_millis(10));

        print!("\x1b[4F\x1b[J");
    }
}
