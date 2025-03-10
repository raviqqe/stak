#!/bin/sh

set -e

[ -n "$CI" ]

brew install comby
cargo install tokei

comby -in-place '... mod tests { ... }' '' .rs
cargo fmt --all

for cargo_file in $(find . -name Cargo.toml | sort); do
  directory=$(dirname $cargo_file)

  echo '>>>' $directory
  (
    cd $directory
    tokei
  )
done
