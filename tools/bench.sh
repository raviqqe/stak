#!/bin/sh

set -ex

brew install chicken gambit-scheme

if [ $(uname -m) = x86_64 ]; then
  brew install chezscheme
fi

cargo install hyperfine
cargo build --release

(
  cd $(dirname $0)/..

  for file in $(find bench -type f -name '*.scm' | sort); do
    base=${file%.scm}

    cat prelude.scm $file | ./main.scm >$base.out

    scripts="target/release/stak $base.out,gsi $file,python3 $base.py,csi -s $file"

    if which petite; then
      scripts="$scripts,petite --script $file"
    fi

    hyperfine --sort command -L script "$scripts" "{script}"
  done
)
