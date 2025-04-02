#!/bin/sh

set -e

[ -n "$CI" ]

. $(dirname $0)/utility.sh

cargo install ast-grep tokei

sg -p 'mod tests { ... }' -r '' --lang rs
sg -p '#[cfg(test)]' -r '' --lang rs
cargo fmt --all

for cargo_file in $(find . -name Cargo.toml | sort); do
  log tokei $(dirname $cargo_file)
done
