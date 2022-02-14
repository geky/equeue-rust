
use criterion::Criterion;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::BatchSize;

use equeue::Equeue;

fn bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("ops");

    {
        // make this massive so we don't have to worry about ooms
        let q = Equeue::with_size(1024*1024*1024);
        group.bench_function("alloc", |b| b.iter_with_large_drop(
            || {
                q.alloc(|| {}).unwrap()
            }
        ));
    }

    {
        // make this massive so we don't have to worry about ooms
        let q = Equeue::with_size(1024*1024*1024);
        group.bench_function("dealloc", |b| b.iter_batched(
            || q.alloc(|| {}).unwrap(),
            |e| {
                drop(e)
            },
            BatchSize::SmallInput
        ));
    }

    {
        // make this massive so we don't have to worry about ooms
        let q = Equeue::with_size(1024*1024*1024);
        group.bench_function("post", |b| b.iter_batched(
            || q.alloc(|| {}).unwrap(),
            |e| {
                e.post_handle()
            },
            BatchSize::SmallInput
        ));
    }

    {
        // make this massive so we don't have to worry about ooms
        let q = Equeue::with_size(1024*1024*1024);
        group.bench_function("cancel", |b| b.iter_batched(
            || q.call(|| {}).unwrap(),
            |id| {
                q.cancel(id)
            },
            BatchSize::SmallInput
        ));
    }

    {
        // we handle dispatch a bit differently to avoid state issues
        group.bench_function("dispatch", |b| b.iter_batched_ref(
            || {
                let q = Equeue::with_size(256);
                q.call(|| {}).unwrap();
                q
            },
            |q| {
                q.dispatch_ready()
            },
            BatchSize::SmallInput
        ));
    }
}

criterion_group!(benches, bench);
criterion_main!(benches);
