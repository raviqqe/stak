#!/bin/sh

set -ex

brew install chibi-scheme gambit-scheme gauche

cargo install hyperfine
cargo build --release

cd $(dirname $0)/..

for file in $(find bench -type f -name '*.scm' | sort); do
  base=${file%.scm}

  cat prelude.scm $file | gosh compile.scm >$base.out

  scripts="target/release/stak $base.out,gsi $file,chibi-scheme $file,gosh $file"

  if [ -r $base.py ]; then
    scripts="$scripts,python3 $base.py"
  fi

  hyperfine --sort command --input compile.scm -L script "$scripts" "{script}"
done
