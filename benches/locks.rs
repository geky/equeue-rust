
use std::time::Instant;
use std::time::Duration;
use std::cmp::max;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::thread;
use std::thread::JoinHandle;
use std::sync::mpsc;

use criterion::Criterion;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Bencher;
use criterion::black_box;

use cfg_if::cfg_if;

use equeue::Equeue;
#[cfg(any(
    equeue_queue_mode="locking",
    equeue_alloc_mode="locking",
    equeue_break_mode="locking",
))]
use equeue::sys::SysLock;


// To measure time locked unintrusively, we sample an internal flag in the lock
// for what percentage of the time the lock is help in another thread.
//
// This isn't perfect, but better than intrusive measurements, which take longer
// than the actual operations we are testing, rendering the results useless...
//
struct SysLockSampler {
    state: Arc<SysLockSamplerState>,
    done: mpsc::Sender<()>,
    thread: Option<JoinHandle<()>>,
}

struct SysLockSamplerState {
    sampling: AtomicBool,
    samples: Mutex<(u64, u64)>,
}

impl SysLockSampler {
    fn new() -> SysLockSampler {
        let (ready_s, ready_r) = mpsc::channel::<()>();
        let (done_s, done_r) = mpsc::channel::<()>();
        let state = Arc::new(SysLockSamplerState {
            sampling: AtomicBool::new(false),
            samples: Mutex::new((0u64, 0u64)),
        });

        let thread = thread::spawn({
            let state = state.clone();
            move || {
                let mut locked_samples = 0u64;
                let mut unlocked_samples = 0u64;

                // indicate we're ready
                ready_s.send(()).unwrap();

                loop {
                    // wait to sample
                    if state.sampling.load(Ordering::SeqCst) {
                        loop {
                            // sample
                            cfg_if! {
                                if #[cfg(any(
                                    equeue_queue_mode="locking",
                                    equeue_alloc_mode="locking",
                                    equeue_break_mode="locking",
                                ))] {
                                    let locked = SysLock::is_locked();
                                } else {
                                    let locked = false;
                                }
                            }

                            // are we sampling?
                            let sampling = state.sampling.load(Ordering::SeqCst);

                            if sampling {
                                if locked {
                                    locked_samples += 1;
                                } else {
                                    unlocked_samples += 1;
                                }
                            } else {
                                break;
                            }

                            thread::yield_now();
                        }

                        // update our measured samples
                        *state.samples.lock().unwrap() = (locked_samples, unlocked_samples);
                    }

                    // but are we done?
                    match done_r.try_recv() {
                        Ok(()) => break,
                        Err(mpsc::TryRecvError::Empty) => continue,
                        Err(err) => panic!("{}", err),
                    }
                }
            }
        });

        // wait for sampler to be ready
        ready_r.recv().unwrap();

        SysLockSampler {
            state: state,
            done: done_s,
            thread: Some(thread),
        }
    }

    fn start(&mut self) {
        self.state.sampling.store(true, Ordering::SeqCst);
    }

    fn stop(&mut self) {
        self.state.sampling.store(false, Ordering::SeqCst);
    }

    fn sampled_ratio(&self) -> f64 {
        assert!(!self.state.sampling.load(Ordering::SeqCst));

        let (locked_samples, unlocked_samples) = *self.state.samples.lock().unwrap();
        if locked_samples+unlocked_samples > 0 {
            (locked_samples as f64) / ((locked_samples+unlocked_samples) as f64)
        } else {
            0.0
        }
    }
}

impl Drop for SysLockSampler {
    fn drop(&mut self) {
        self.done.send(()).unwrap();
        self.state.sampling.store(false, Ordering::SeqCst);
        self.thread.take().unwrap().join().unwrap();
    }
}

// add iter_with_sys_lock_sampling
trait BencherExt {
    fn iter_with_sys_lock_sampling<I, O, S, R>(
        &mut self,
        setup: S,
        routine: R,
    )
    where
        S: FnMut() -> I,
        R: FnMut(I) -> O;

    fn iter_with_sys_lock_sampling_ref<I, O, S, R>(
        &mut self,
        setup: S,
        routine: R,
    )
    where
        S: FnMut() -> I,
        R: FnMut(&mut I) -> O;
}

impl BencherExt for Bencher<'_> {
    fn iter_with_sys_lock_sampling<I, O, S, R>(
        &mut self,
        mut setup: S,
        mut routine: R,
    )
    where
        S: FnMut() -> I,
        R: FnMut(I) -> O
    {
        self.iter_custom(|iters| {
            let mut inputs = Vec::with_capacity(usize::try_from(iters).unwrap());
            let mut outputs = Vec::with_capacity(usize::try_from(iters).unwrap());
            let mut sampler = SysLockSampler::new();
            for _ in 0..iters {
                inputs.push(setup());
            }

            let now = Instant::now();
            sampler.start();
            for input in inputs {
                outputs.push(black_box(
                    routine(input)
                ));
            }
            sampler.stop();
            let dur = now.elapsed();

            drop(outputs);
            max(
                dur.mul_f64(sampler.sampled_ratio()),
                // required by Criterion
                Duration::from_nanos(1),
            )
        })
    }

    fn iter_with_sys_lock_sampling_ref<I, O, S, R>(
        &mut self,
        mut setup: S,
        mut routine: R,
    )
    where
        S: FnMut() -> I,
        R: FnMut(&mut I) -> O
    {
        self.iter_custom(|iters| {
            let mut inputs = Vec::with_capacity(usize::try_from(iters).unwrap());
            let mut outputs = Vec::with_capacity(usize::try_from(iters).unwrap());
            let mut sampler = SysLockSampler::new();
            for _ in 0..iters {
                inputs.push(setup());
            }

            let now = Instant::now();
            sampler.start();
            for input in inputs.iter_mut() {
                outputs.push(black_box(
                    routine(input)
                ));
            }
            sampler.stop();
            let dur = now.elapsed();

            drop(inputs);
            drop(outputs);
            max(
                dur.mul_f64(sampler.sampled_ratio()),
                // required by Criterion
                Duration::from_nanos(1),
            )
        })
    }
}


// actual benchmarks
fn bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("locks");

    {
        // make this massive so we don't have to worry about ooms
        let q = Equeue::with_size(1024*1024*1024);
        group.bench_function("alloc", |b| b.iter_with_sys_lock_sampling(
            || (),
            |_| {
                q.alloc(|| {}).unwrap()
            },
        ));
    }

    {
        // make this massive so we don't have to worry about ooms
        let q = Equeue::with_size(1024*1024*1024);
        group.bench_function("dealloc", |b| b.iter_with_sys_lock_sampling(
            || q.alloc(|| {}).unwrap(),
            |e| {
                drop(e)
            },
        ));
    }

    {
        // make this massive so we don't have to worry about ooms
        let q = Equeue::with_size(1024*1024*1024);
        group.bench_function("post", |b| b.iter_with_sys_lock_sampling(
            || q.alloc(|| {}).unwrap(),
            |e| {
                e.post_handle()
            },
        ));
    }

    {
        // make this massive so we don't have to worry about ooms
        let q = Equeue::with_size(1024*1024*1024);
        group.bench_function("cancel", |b| b.iter_with_sys_lock_sampling(
            || q.call(|| {}).unwrap(),
            |id| {
                q.cancel(id)
            },
        ));
    }

    {
        // we handle dispatch a bit differently to avoid state issues
        group.bench_function("dispatch", |b| b.iter_with_sys_lock_sampling_ref(
            || {
                let q = Equeue::with_size(256);
                q.call(|| {}).unwrap();
                q
            },
            |q| {
                q.dispatch_ready()
            },
        ));
    }
}

criterion_group!(benches, bench);
criterion_main!(benches);
