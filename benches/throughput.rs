
use std::thread;
use std::sync::Arc;
use std::sync::mpsc;
use std::sync::Mutex;
use std::iter;
use std::env;

use criterion::Criterion;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Throughput;
use criterion::SamplingMode;
use criterion::BenchmarkId;

use equeue::Equeue;

fn bench(c: &mut Criterion) {
    // environment variables
    #[allow(non_snake_case)]
    let EQUEUE_THROUGHPUT_COUNT = env::var("EQUEUE_THROUGHPUT_COUNT")
        .map(|throughput_count| throughput_count.parse().unwrap())
        .unwrap_or(1000000);

    #[allow(non_snake_case)]
    let EQUEUE_THROUGHPUT_CORES = env::var("EQUEUE_THROUGHPUT_CORES")
        .map(|throughput_cores| throughput_cores.parse().unwrap())
        .unwrap_or(4);

    // start dispatch thread
    let q = Arc::new(Equeue::with_size(1024*1024*1024));
    let dispatch_thread = thread::spawn({
        let q = q.clone();
        move || {
            q.dispatch();
        }
    });

    for n in
        iter::successors(Some(1), |n| Some(n*2))
            .take_while(|&n| n < EQUEUE_THROUGHPUT_CORES)
            .chain(iter::once(EQUEUE_THROUGHPUT_CORES))
            .chain(iter::once(2*EQUEUE_THROUGHPUT_CORES))
            .chain(iter::once(4*EQUEUE_THROUGHPUT_CORES))
    {
        // setup threads
        let mut threads = vec![];
        let mut kicks = vec![];
        let mut dones = vec![];
        for _ in 0..n {
            let (kick_s, kick_r) = mpsc::channel::<bool>();
            let (done_s, done_r) = mpsc::channel::<()>();
            let done_s = Mutex::new(done_s);
            kicks.push(kick_s);
            dones.push(done_r);
            threads.push(thread::spawn({
                let q = q.clone();
                move || {
                    while kick_r.recv().unwrap() {
                        for _ in 0..EQUEUE_THROUGHPUT_COUNT/n {
                            q.call(|| {}).unwrap();
                        }

                        done_s.lock().unwrap().send(()).unwrap();
                    }
                }
            }));
        }

        // benchmark
        let mut group = c.benchmark_group("throughput");
        group.sample_size(10);
        group.sampling_mode(SamplingMode::Flat);
        group.throughput(Throughput::Elements(EQUEUE_THROUGHPUT_COUNT));

        group.bench_function(BenchmarkId::new("throughput", n), |b| b.iter(|| {
            for kick in kicks.iter() {
                kick.send(true).unwrap();
            }

            for done in dones.iter() {
                done.recv().unwrap();
            }
        }));

        // join threads
        for kick in kicks.iter() {
            kick.send(false).unwrap();
        }

        for thread in threads.drain(..) {
            thread.join().unwrap();
        }
    }

    // join dispatch thread
    q.break_();
    dispatch_thread.join().unwrap();
}

criterion_group!(benches, bench);
criterion_main!(benches);
