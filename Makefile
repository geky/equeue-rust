
.PHONY: all build
all build:
	cargo build

.PHONY: test
test:
	cargo test --tests -- --test-threads 1

.PHONY: clean
clean:
	cargo clean
