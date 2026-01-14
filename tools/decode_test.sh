#!/bin/sh

set -e

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

cargo build --profile release_test

export PATH=$PWD/target/release_test:$PATH

decode() (
  file=$1

  echo FILE $file

  base=snapshots/${file%.scm}

  mkdir -p $(dirname $base)
  cat prelude.scm $file | stak-compile >$base.bc
  stak-decode <$base.bc >$base.md
)

list_scheme_files | parallel decode

pnpm oxfmt snapshots
git diff --exit-code
