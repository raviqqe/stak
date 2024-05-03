#!/bin/sh

set -e

cargo --version
cargo clippy "$@"
cargo clippy --all-features "$@"
