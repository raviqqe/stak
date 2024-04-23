#!/bin/sh

set -e

cargo clippy "$@" -- -D warnings -D clippy::use_self
cargo clippy --all-features "$@" -- -D warnings -D clippy::use_self
