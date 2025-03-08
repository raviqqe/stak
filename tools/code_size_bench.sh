#!/bin/sh

set -e

[ -n "$CI" ]

brew install comby

comby -in-place 'mod tests { ... }' '' .rs
cargo install tokei
cargo fmt --all

for cargo_file in $(find . -name Cargo.toml); do
  (
    cd $(dirname $cargo_file)
    tokei
  )
done
