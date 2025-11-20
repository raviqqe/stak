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
    stak-lzss compress |
    stak-decode >$base.md
done

npx prettier --write snapshots
git diff --exit-code
