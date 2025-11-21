#!/bin/sh

set -e

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

cargo build --release

export PATH=$PWD/target/release:$PATH

for file in $(list_scheme_files); do
  echo FILE $file

  base=snapshots/${file%.scm}

  mkdir -p $(dirname $base)
  cat prelude.scm $file |
    stak-compile --shake-tree |
    stak-lzss right-shift 1 |
    stak-lzss compress |
    stak-lzss decompress |
    stak-lzss left-shift 1 >$base.bc
  stak-decode <$base.bc >$base.md
done

npx prettier --write snapshots
git diff --exit-code
