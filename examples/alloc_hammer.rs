
use equeue::Equeue;

use std::thread;
use std::alloc::Layout;
use std::mem::transmute;
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

    let mut buffer = vec![0; 1024*1024];
    let q = Arc::new(Equeue::with_buffer(
        unsafe { transmute::<&mut [u8], &'static mut [u8]>(buffer.as_mut()) }
    ).unwrap());

    // some busywork, n threads, each randomly allocating some random size for
    // some random about of time
    let mut threads = vec![];
    for _ in 0..200 {
        let opt = opt.clone();
        let q = q.clone();
        threads.push(thread::spawn(move || {
            let mut rng = rand::thread_rng();
            loop {
                thread::sleep(Duration::from_nanos(rng.gen_range(0..1000*opt.scale)));

                let layout = Layout::from_size_align(rng.gen_range(1..8192), 1).unwrap();
                let e = unsafe { q.alloc_raw(layout) };

                thread::sleep(Duration::from_nanos(rng.gen_range(0..1000*opt.scale)));

                unsafe { q.dealloc_raw(e, layout) };
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

        print!("\x1b[4F");
    }
}
