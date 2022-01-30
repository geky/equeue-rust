
use equeue::Equeue;

use std::thread;
use std::alloc::Layout;
use std::sync::Arc;
use std::time::Duration;
use std::cmp::max;

use rand;
use rand::Rng;
use structopt;
use structopt::StructOpt;


#[derive(Debug, StructOpt, Clone)]
#[structopt(rename_all="kebab")]
struct Opt {
    /// Width of rendering (excluding fields)
    #[structopt(short, long, default_value="48")]
    width: usize,

    /// Amount to scale delays
    #[structopt(short, long, default_value="1000000")]
    scale: u64,
}

fn main() {
    let opt = Opt::from_args();

    let q = Arc::new(Equeue::with_size(1024*1024));

    let mut threads = vec![];

    // one dispatch thread
    {
        let q = q.clone();
        threads.push(thread::spawn(move || {
            q.dispatch_forever();
            unreachable!();
        }));
    }

    // some busywork, n threads, each a random size, and a random delay
    for _ in 0..100 {
        let opt = opt.clone();
        let q = q.clone();
        threads.push(thread::spawn(move || {
            let mut rng = rand::thread_rng();
            loop {
                thread::sleep(Duration::from_nanos(rng.gen_range(0..2000*opt.scale)));

                // choose a random size, we need to use the raw APIs for dynamic sizes
                let layout = Layout::from_size_align(rng.gen_range(1..8192), 1).unwrap();
                let e = unsafe { q.alloc_raw(layout) };
                if e.is_null() {
                    thread::sleep(Duration::from_nanos(rng.gen_range(0..2000*opt.scale)));
                    continue;
                }

                fn cb(_data: *mut u8) {
                    // do nothing
                }

                // choose a random delay
                let delay = rng.gen_range(0..max(2000*opt.scale/1000_000, 10));
                unsafe { q.set_raw_delay(e, Duration::from_millis(delay)) };

                let id = unsafe { q.post_raw(cb, e) };

                // should we try to cancel?
                if rng.gen() {
                    // wait a random time and then cancel, our event may have already
                    // executed but that's ok
                    thread::sleep(Duration::from_nanos(rng.gen_range(0..2000*opt.scale)));
                    q.cancel(id);
                }
            }
        }));
    }

    // and now, in our main thread, lets render something nice looking
    println!();

    loop {
        let usage = q.usage();
        let mut slices = vec![0; usage.slices];
        let mut buckets = vec![0; usage.buckets];
        q.slice_usage(&mut slices);
        q.bucket_usage(&mut buckets);

        let mut used_buckets = 0;
        for &bucket in buckets.iter() {
            if bucket != 0 {
                used_buckets += 1;
            }
        }

        // render this thing
        let print_slices = |row: usize| {
            for &slice in slices.iter().take(opt.width-2) {
                let slice_dots = slice;
                if slice_dots > 2*row+1 {
                    print!(":");
                } else if slice_dots > 2*row {
                    print!("'");
                } else {
                    print!(" ");
                }
            }

            for _ in slices.len() .. opt.width-2 {
                print!(" ");
            }
        };

        print!("\x1b[K  q "); print_slices(0); println!("  pending: {} ({} B)", usage.pending, usage.pending_bytes);
        print!("\x1b[K    "); print_slices(1); println!("  alloced: {} ({} B)", usage.alloced, usage.alloced_bytes);
        print!("\x1b[K    "); print_slices(2); println!("  free: {} ({} B)", usage.free, usage.free_bytes);

        let print_buckets = |row: usize| {
            for &bucket in buckets.iter().take((opt.width-2)/2) {
                let bucket_dots = bucket;
                if bucket_dots > 4*row+3 {
                    print!("::");
                } else if bucket_dots > 4*row+2 {
                    print!(":.");
                } else if bucket_dots > 4*row+1 {
                    print!("..");
                } else if bucket_dots > 4*row {
                    print!(". ");
                } else {
                    print!("  ");
                }
            }

            for _ in 2*buckets.len() .. opt.width-2 {
                print!(" ");
            }
        };

        print!("\x1b[K    "); print_buckets(2); println!("  slices: {}", usage.slices);
        print!("\x1b[K    "); print_buckets(1); println!();
        print!("\x1b[K  f "); print_buckets(0); println!("  slab: {}/{}", usage.slab_unused, usage.slab_total);

        print!("\x1b[K  [");
        for _ in 0
            .. (opt.width-2)*usage.pending_bytes / usage.slab_total
        {
            print!("|");
        }
        for _ in (opt.width-2)*usage.pending_bytes / usage.slab_total
            .. (opt.width-2)*(usage.pending_bytes+usage.alloced_bytes) / usage.slab_total
        {
            print!("#");
        }
        for _ in (opt.width-2)*(usage.alloced_bytes+usage.pending_bytes) / usage.slab_total
            .. (opt.width-2)*usage.slab_fragmented / usage.slab_total
        {
            print!(":");
        }
        for _ in (opt.width-2)*usage.slab_fragmented / usage.slab_total
            .. (opt.width-2)
        {
            print!(" ");
        }
        print!("]");
        println!("  buckets: {}/{}", used_buckets, usage.buckets);

        thread::sleep(Duration::from_millis(10));

        print!("\x1b[7F");
    }
}
