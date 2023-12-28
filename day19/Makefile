.PHONY: all
all: run-haskell

.PHONY: build-haskell
build-haskell:
	cabal build

.PHONY: run-haskell
run-haskell: build-haskell
	time cabal exec day19
