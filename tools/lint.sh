#!/bin/sh

set -e

cargo clippy "$@"
cargo clippy --all-features "$@"
