#!/bin/sh

set -e

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

cargo build --release

export PATH=$PWD/target/release:$PATH

for file in $(list_scheme_files); do
  echo '>>>' $file
  snapshot_file=snapshots/${file%.scm}.md
  mkdir -p $(dirname $snapshot_file)
  cat prelude.scm $file | stak-compile >main.bc
  stak-decode <main.bc >$snapshot_file
done

npx prettier --write snapshots
git diff --exit-code
