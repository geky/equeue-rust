
.PHONY: all build
all build:
	cargo build

.PHONY: test
test:
	cargo test --tests -- --test-threads 1 --nocapture

.PHONY: test-release
test-release:
	cargo test --release --tests -- --test-threads 1 --nocapture

.PHONY: clean
clean:
	cargo clean
