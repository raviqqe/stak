#!/bin/sh

set -ex

brew install gambit-scheme
cargo install hyperfine
cargo build --release --features "$FEATURES"

for file in $(find bench -type f -name '*.scm'); do
  base=${file%.scm}

  cat prelude.scm $file | tools/compile.sh >$base.out

  hyperfine \
    --sort command \
    "target/release/stak $base.out" \
    "gsi $file" \
    "python3 $base.py"
done
