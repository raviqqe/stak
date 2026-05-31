#!/bin/sh

set -e

cargo clippy --all-targets "$@"
cargo clippy --all-targets --all-features "$@"
