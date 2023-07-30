#!/bin/sh

set -ex

cargo install hyperfine

for file in $(find bench -type f -name '*.scm'); do
  base=${file%.scm}

  cat prelude.scm $file | tools/compile.sh >$base.out

  hyperfine \
    "target/release/stak $base.out" \
    "gsi $file" \
    "python3 $base.py"
done
