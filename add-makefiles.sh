#!/bin/bash

# Loop over all directories matching the pattern 'dayNN'
for dir in day[0-9][0-9]; do
    # Check if the directory exists and is indeed a directory
    if [ -d "$dir" ]; then
        has_cabal_file=false
        has_cargo_file=false

        # Check for the existence of 'dayNN.cabal' and 'Cargo.toml' files within the directory
        [ -f "${dir}/${dir}.cabal" ] && has_cabal_file=true
        [ -f "${dir}/Cargo.toml" ] && has_cargo_file=true

        if $has_cabal_file || $has_cargo_file; then
            # Initialize Makefile
            echo ".PHONY: all" > "${dir}/Makefile"

            # Add Haskell targets if a cabal file is present
            if $has_cabal_file; then
                {
                    echo "all: run-haskell"
                    echo
                    echo ".PHONY: build-haskell"
                    echo "build-haskell:"
                    echo $'\t'"cabal build"
                    echo
                    echo ".PHONY: run-haskell"
                    echo "run-haskell: build-haskell"
                    echo $'\t'"time cabal exec ${dir}"
                } >> "${dir}/Makefile"
            fi

            # Add Rust targets if a Cargo.toml file is present
            if $has_cargo_file; then
                # Specify the default target
                if ! $has_cabal_file; then
                    echo "all: run-rust" >> "${dir}/Makefile"
                fi

                {
                    echo
                    echo ".PHONY: build-rust"
                    echo "build-rust:"
                    echo $'\t'"cargo build --release"
                    echo
                    echo ".PHONY: run-rust"
                    echo "run-rust: build-rust"
                    echo $'\t'"time cargo run --release"
                } >> "${dir}/Makefile"
            fi
        fi
    fi
done
