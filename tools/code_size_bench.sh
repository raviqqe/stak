#!/bin/sh

set -e

[ -n "$CI" ]

. $(dirname $0)/utility.sh

brew install comby
cargo install tokei

comby -in-place '... mod tests { ... }' '' .rs
cargo fmt --all

for cargo_file in $(find . -name Cargo.toml | sort); do
  log tokei $(dirname $cargo_file)
done
