#!/bin/sh

set -e

[ -n "$CI" ]

. $(dirname $0)/utility.sh

cargo install ast-grep tokei

for pattern in 'mod tests { $$$ }' '#[cfg(test)]'; do
  sg -Ul rs -p "$pattern" -r ''
done

cargo fmt --all

tokei

for cargo_file in $(find . -name Cargo.toml | sort); do
  log tokei $(dirname $cargo_file)
done
