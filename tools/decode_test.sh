#!/bin/sh

set -e

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

cargo build --profile release_test

export PATH=$PWD/target/release_test:$PATH

for file in $(list_scheme_files); do
  echo FILE $file

  base=snapshots/${file%.scm}

  mkdir -p $(dirname $base)
  cat prelude.scm $file | stak-compile --shake-tree >$base.bc
  stak-decode <$base.bc >$base.md
done

npx prettier --write snapshots
git diff --exit-code
