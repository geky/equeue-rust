
use equeue::Equeue;
use equeue::Event;

use std::thread;
use std::alloc::Layout;
use std::sync::Arc;
use std::time::Duration;

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
        let opt = opt.clone();
        let q = q.clone();
        threads.push(thread::spawn(move || {
            loop {
                q.dispatch_ready();
                thread::sleep(Duration::from_nanos(1000*opt.scale));
            }
        }));
    }

    // some busywork, n threads, each a random size
    for _ in 0..100 {
        let opt = opt.clone();
        let q = q.clone();
        threads.push(thread::spawn(move || {
            let mut rng = rand::thread_rng();
            loop {
                thread::sleep(Duration::from_nanos(rng.gen_range(0..2000*opt.scale)));

                // choose a random size, we need to use the raw APIs for dynamic sizes
                let layout = Layout::from_size_align(rng.gen_range(1..8192), 1).unwrap();
                let e = unsafe { q.alloc_raw(layout, |_|{}, |_|{}) };
                if e.is_null() {
                    thread::sleep(Duration::from_nanos(rng.gen_range(0..2000*opt.scale)));
                    continue;
                }
                unsafe { Event::from_raw(&q, e) }
                    .post();
            }
        }));
    }

    // and now, in our main thread, lets render something nice looking
    println!();

    loop {
        let usage = q.usage();
        let mut buckets = vec![0; usage.buckets];
        q.bucket_usage(&mut buckets);

        let mut used_buckets = 0;
        for &bucket in buckets.iter() {
            if bucket != 0 {
                used_buckets += 1;
            }
        }

        // render this thing
        print!("\x1b[K  q ");
        for i in 0..opt.width-2 {
            if usage.posted > 2*i+1 {
                print!(":");
            } else if usage.posted > 2*i {
                print!("'");
            } else {
                print!(" ");
            }
        }
        println!("  posted: {} ({} B)", usage.posted, usage.posted_bytes);

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

        print!("\x1b[K    "); print_buckets(2); println!("  alloced: {} ({} B)", usage.alloced, usage.alloced_bytes);
        print!("\x1b[K    "); print_buckets(1); println!("  free: {} ({} B)", usage.free, usage.free_bytes);
        print!("\x1b[K  f "); print_buckets(0); println!("  slab: {}/{}", usage.slab_unused, usage.slab_total);

        print!("\x1b[K  [");
        for _ in 0
            .. (opt.width-2)*usage.posted_bytes / usage.slab_total
        {
            print!("|");
        }
        for _ in (opt.width-2)*usage.posted_bytes / usage.slab_total
            .. (opt.width-2)*(usage.posted_bytes+usage.alloced_bytes) / usage.slab_total
        {
            print!("#");
        }
        for _ in (opt.width-2)*(usage.alloced_bytes+usage.posted_bytes) / usage.slab_total
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

        print!("\x1b[5F");
    }
}
