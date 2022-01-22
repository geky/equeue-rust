
.PHONY: all build
all build:
	cargo build

.PHONY: test
test:
	cargo test --tests

.PHONY: build-configs
build-configs:
	cargo build --no-default-features
	cargo build --no-default-features --features alloc
	cargo build
	cargo build --features async-io
	cargo build --features async-std
	cargo build --features tokio

.PHONY: test-configs
test-configs: build-configs
	cargo test --tests
	cargo test --tests --features async-io
	cargo test --tests --features async-std
	cargo test --tests --features tokio

.PHONY: docs
docs:
	cargo doc --no-deps

.PHONY: clean
clean:
	cargo clean
