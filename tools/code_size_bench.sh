#!/bin/sh

set -e

[ -n "$CI" ]

brew install comby

comby -in-place 'mod tests { ... }' '' .rs
cargo install tokei
cargo fmt --all

for cargo_file in $(find . -name Cargo.toml); do
  directory=$(dirname $cargo_file)

  echo '>>>' $directory
  (
    cd $directory
    tokei
  )
done
