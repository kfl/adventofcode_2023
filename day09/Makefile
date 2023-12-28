.PHONY: all
all: run-haskell

.PHONY: build-haskell
build-haskell:
	cabal build

.PHONY: run-haskell
run-haskell: build-haskell
	time cabal exec day09

.PHONY: build-rust
build-rust:
	cargo build --release

.PHONY: run-rust
run-rust: build-rust
	time cargo run --release
