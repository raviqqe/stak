#!/bin/sh

set -e

rustup component add llvm-tools-preview

cargo install --lock cargo-llvm-cov
cargo llvm-cov --workspace --profile release --lcov --output-path lcov.info
