
.PHONY: all build
all build:
	cargo build
	cargo build --tests
	cargo build --examples

.PHONY: build-configs
build-configs:
	cargo build --no-default-features
	cargo build --no-default-features --features alloc
	cargo build
	cargo build --features loom
	cargo build --release
	cargo build --features embedded-time
	cargo build --features utick-at-least-u128
	EQUEUE_FREQUENCY=1000000000 cargo build --features utick-at-least-u64,embedded-time
	cargo build --features async-io
	cargo build --features async-std
	cargo build --features tokio

.PHONY: test
test:
	cargo test --tests
	LOOM_MAX_PREEMPTIONS=1 cargo test --test loom --features loom

.PHONY: test-loom
test-loom:
	LOOM_MAX_PREEMPTIONS=2 cargo test --test loom --features loom

.PHONY: test-configs
test-configs: build-configs
	cargo test --tests
	cargo test --tests --release
	cargo test --tests --features embedded-time
	cargo test --tests --features utick-at-least-u128
	EQUEUE_FREQUENCY=1000000000 cargo test --tests --features utick-at-least-u64,embedded-time
	cargo test --tests --features async-io
	cargo test --tests --features async-std
	cargo test --tests --features tokio

.PHONY: bench
bench:
	mkdir -p target/bench
	cargo bench --features criterion --benches -- --noplot
	$(strip \
		awk '(NR == 1) || (FNR > 1)' \
		$$(find -path './target/criterion/*/new/raw.csv') \
		> target/bench/lockless.csv )
	$(strip \
		EQUEUE_QUEUE_MODE=locking \
		EQUEUE_ALLOC_MODE=lockless \
		EQUEUE_BREAK_MODE=lockless \
		cargo bench --features criterion --benches -- --noplot )
	$(strip \
		awk '(NR == 1) || (FNR > 1)' \
		$$(find -path './target/criterion/*/new/raw.csv') \
		> target/bench/lockless-alloc-break.csv )
	$(strip \
		EQUEUE_QUEUE_MODE=locking \
		EQUEUE_ALLOC_MODE=lockless \
		cargo bench --features criterion --benches -- --noplot )
	$(strip \
		awk '(NR == 1) || (FNR > 1)' \
		$$(find -path './target/criterion/*/new/raw.csv') \
		> target/bench/lockless-alloc.csv )
	$(strip \
		EQUEUE_QUEUE_MODE=locking \
		cargo bench --features criterion --benches -- --noplot )
	$(strip \
		awk '(NR == 1) || (FNR > 1)' \
		$$(find -path './target/criterion/*/new/raw.csv') \
		> target/bench/locking.csv )

.PHONY: graph
graph:
	$(strip ./scripts/graph-throughput.py target/bench/throughput.svg \
		locking=target/bench/locking.csv \
		lockless-alloc=target/bench/lockless-alloc.csv \
		lockless-alloc-break=target/bench/lockless-alloc-break.csv \
		lockless=target/bench/lockless.csv )

.PHONY: docs
docs:
	cargo doc --no-deps

.PHONY: clean
clean:
	cargo clean
