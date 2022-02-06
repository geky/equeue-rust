
.PHONY: all build
all build:
	cargo build
	cargo build --tests
	cargo build --examples

.PHONY: test
test:
	cargo test --tests

.PHONY: test-loom
test-loom:
	LOOM_MAX_PREEMPTIONS=2 cargo test --test test_loom --features loom

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

.PHONY: docs
docs:
	cargo doc --no-deps

.PHONY: clean
clean:
	cargo clean
