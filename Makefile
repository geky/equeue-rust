
.PHONY: all build
all build:
	cargo build

.PHONY: test
test:
	cargo test --tests

.PHONY: docs
docs:
	cargo doc --no-deps

.PHONY: clean
clean:
	cargo clean
