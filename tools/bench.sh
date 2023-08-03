#!/bin/sh

set -ex

brew install chezscheme chicken gambit-scheme
cargo install hyperfine

cargo build --release

(
  cd $(dirname $0)/..

  for file in $(find bench -type f -name '*.scm' | sort); do
    base=${file%.scm}

    cat prelude.scm $file | ./main.scm >$base.out

    hyperfine \
      --sort command \
      "target/release/stak $base.out" \
      "gsi $file" \
      "python3 $base.py" \
      "petite -s $file" \
      "csi -s $file"
  done
)
