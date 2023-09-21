#!/bin/sh

set -ex

brew install chibi-scheme gambit-scheme gauche

cargo install hyperfine
cargo build --release

cd $(dirname $0)/..

for file in $(find bench -type f -name '*.scm' | sort); do
  base=${file%.scm}

  cat prelude.scm $file | ./compile.scm >$base.out

  hyperfine \
    --sort command \
    -L script "chibi-scheme $file,target/release/stak $base.out,gsi $file,python3 $base.py,gosh $file" \
    "{script}"
done
