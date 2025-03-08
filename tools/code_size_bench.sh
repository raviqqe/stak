#!/bin/sh

set -e

[ -n "$CI" ]

brew install comby
cargo install tokei

comby -in-place '#[cfg(test)] mod tests { ... }' '' .rs
cargo fmt --all

for cargo_file in $(find . -name Cargo.toml); do
  directory=$(dirname $cargo_file)

  echo '>>>' $directory
  (
    cd $directory
    tokei
  )
done
