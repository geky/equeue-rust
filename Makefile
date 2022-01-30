
.PHONY: all build
all build:
	cargo build --features raw
	cargo build --tests --features raw
	cargo build --examples --features raw

.PHONY: test
test:
	cargo test --tests --features raw

.PHONY: build-configs
build-configs:
	cargo build --no-default-features
	cargo build --no-default-features --features alloc
	cargo build
	cargo build --features raw
	cargo build --features raw --release
	cargo build --features raw,embedded-time
	cargo build --features raw,utick-at-least-u128
	EQUEUE_FREQUENCY=1000000000 cargo build --features raw,utick-at-least-u64,embedded-time
	cargo build --features raw,async-io
	cargo build --features raw,async-std
	cargo build --features raw,tokio

.PHONY: test-configs
test-configs: build-configs
	cargo test --tests --features raw
	cargo test --tests --features raw --release
	cargo test --tests --features raw,embedded-time
	cargo test --tests --features raw,utick-at-least-u128
	EQUEUE_FREQUENCY=1000000000 cargo test --tests --features raw,utick-at-least-u64,embedded-time
	cargo test --tests --features raw,async-io
	cargo test --tests --features raw,async-std
	cargo test --tests --features raw,tokio

.PHONY: docs
docs:
	cargo doc --no-deps

.PHONY: clean
clean:
	cargo clean
