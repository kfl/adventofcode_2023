.PHONY: all
all: run-rust

.PHONY: build-rust
build-rust:
	cargo build --release

.PHONY: run-rust
run-rust: build-rust
	time cargo run --release
