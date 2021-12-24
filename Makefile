
.PHONY: all build
all build:
	cargo build

.PHONY: test
test:
	cargo test --tests

.PHONY: clean
clean:
	cargo clean
