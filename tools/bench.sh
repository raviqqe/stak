#!/bin/sh

set -ex

brew install chibi-scheme gambit-scheme gauche

cargo install hyperfine
cargo build --release

cd $(dirname $0)/..

export PATH=$PWD/target/release:$PATH

filter=.

if [ $# -gt 0 ]; then
  filter="$@"
fi

for file in $(find bench -type f -name '*.scm' | sort | grep $filter); do
  base=${file%.scm}

  cat prelude.scm $file | gosh compile.scm >$base.out

  scripts="stak-interpret $base.out,gsi $file,chibi-scheme $file,gosh $file"

  if [ -r $base.py ]; then
    scripts="$scripts,python3 $base.py"
  fi

  hyperfine --sort command --input compile.scm -L script "$scripts" "{script}"
done
