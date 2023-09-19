#!/bin/sh

set -ex

brew install chicken gambit-scheme gauche

if [ $(uname -m) = x86_64 ]; then
  # spell-checker: disable-next-line
  brew install chezscheme
fi

cargo install hyperfine
cargo build --release

(
  cd $(dirname $0)/..

  for file in $(find bench -type f -name '*.scm' | sort); do
    base=${file%.scm}

    cat prelude.scm $file | ./compile.scm >$base.out

    scripts="target/release/stak $base.out,gsi $file,python3 $base.py,csi -s $file,gosh $file"

    if which petite; then
      scripts="$scripts,petite --script $file"
    fi

    hyperfine --sort command -L script "$scripts" "{script}"
  done
)
