
.PHONY: all build
all build:
	cargo build
	cargo build --tests --benches --examples

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

.PHONY: size
size:
	$(strip cargo bloat \
		--no-default-features \
		--profile small \
		--example code_size \
		--filter equeue \
		--message-format json \
		-n 0 \
		| jq -r '.functions | map(select(.name | contains("Debug") | not))'" \
			"'| reverse | .[] | "\(.size) \(.name)"' \
		| awk 'NR==1 {printf "%7s %s\n","text","function"}'" \
			"'{sum+=$$1; printf "%7d %s\n",$$1,substr($$0,length($$1)+2)}'" \
			"'END {printf "%7d %s\n",sum,"TOTAL"}' )

.PHONY: bench-code-size
bench-code-size:
	mkdir -p target/bench/code-size
	$(strip \
		cargo bloat \
		--no-default-features \
		--profile small \
		--example code_size \
		--filter equeue \
		--message-format json \
		-n 0 \
		| jq -r '.functions | map(select(.name | contains("Debug") | not))'" \
			"'| (.[0] | keys), (.[] | map(values)) | @csv' \
		> target/bench/code-size/lockless.csv)
	$(strip \
		EQUEUE_QUEUE_MODE=locking \
		EQUEUE_ALLOC_MODE=lockless \
		EQUEUE_BREAK_MODE=lockless \
		cargo bloat \
		--no-default-features \
		--profile small \
		--example code_size \
		--filter equeue \
		--message-format json \
		-n 0 \
		| jq -r '.functions | map(select(.name | contains("Debug") | not))'" \
			"'| (.[0] | keys), (.[] | map(values)) | @csv' \
		> target/bench/code-size/lockless-alloc.csv)
	$(strip \
		EQUEUE_QUEUE_MODE=locking \
		EQUEUE_ALLOC_MODE=locking \
		EQUEUE_BREAK_MODE=lockless \
		cargo bloat \
		--no-default-features \
		--profile small \
		--example code_size \
		--filter equeue \
		--message-format json \
		-n 0 \
		| jq -r '.functions | map(select(.name | contains("Debug") | not))'" \
			"'| (.[0] | keys), (.[] | map(values)) | @csv' \
		> target/bench/code-size/lockless-break.csv)
	$(strip \
		EQUEUE_MODE=locking \
		cargo bloat \
		--no-default-features \
		--profile small \
		--example code_size \
		--filter equeue \
		--message-format json \
		-n 0 \
		| jq -r '.functions | map(select(.name | contains("Debug") | not))'" \
			"'| (.[0] | keys), (.[] | map(values)) | @csv' \
		> target/bench/code-size/locking.csv)

.PHONY: bench-throughput
bench-throughput:
	mkdir -p target/bench/throughput
	$(strip \
		cargo bench --features criterion --benches -- --noplot )
	$(strip \
		awk '(NR == 1) || (FNR > 1)' \
		$$(find -path './target/criterion/*/new/raw.csv') \
		> target/bench/throughput/lockless.csv )
	$(strip \
		EQUEUE_QUEUE_MODE=locking \
		EQUEUE_ALLOC_MODE=lockless \
		EQUEUE_BREAK_MODE=lockless \
		cargo bench --features criterion --benches -- --noplot )
	$(strip \
		awk '(NR == 1) || (FNR > 1)' \
		$$(find -path './target/criterion/*/new/raw.csv') \
		> target/bench/throughput/lockless-alloc.csv )
	$(strip \
		EQUEUE_QUEUE_MODE=locking \
		EQUEUE_ALLOC_MODE=locking \
		EQUEUE_BREAK_MODE=lockless \
		cargo bench --features criterion --benches -- --noplot )
	$(strip \
		awk '(NR == 1) || (FNR > 1)' \
		$$(find -path './target/criterion/*/new/raw.csv') \
		> target/bench/throughput/lockless-break.csv )
	$(strip \
		EQUEUE_MODE=locking \
		cargo bench --features criterion --benches -- --noplot )
	$(strip \
		awk '(NR == 1) || (FNR > 1)' \
		$$(find -path './target/criterion/*/new/raw.csv') \
		> target/bench/throughput/locking.csv )

.PHONY: bench
bench: bench-code-size bench-throughput

.PHONY: graph-code-size
graph-code-size:
	$(strip ./scripts/graph-code-size.py target/bench/code-size.svg \
		locking=target/bench/code-size/locking.csv \
		lockless-break=target/bench/code-size/lockless-break.csv \
		lockless-alloc=target/bench/code-size/lockless-alloc.csv \
		lockless=target/bench/code-size/lockless.csv )

.PHONY: graph-throughput
graph-throughput:
	$(strip ./scripts/graph-throughput.py target/bench/throughput.svg \
		locking=target/bench/throughput/locking.csv \
		lockless-break=target/bench/throughput/lockless-break.csv \
		lockless-alloc=target/bench/throughput/lockless-alloc.csv \
		lockless=target/bench/throughput/lockless.csv )

.PHONY: graph
graph: graph-code-size graph-throughput

.PHONY: docs
docs:
	cargo doc --no-deps

.PHONY: clean
clean:
	cargo clean
