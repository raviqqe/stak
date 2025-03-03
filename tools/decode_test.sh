#!/bin/sh

set -e

cargo_run() {
  command=$1
  shift 1
  cargo run --release --bin $command -- $@
}

cd $(dirname $0)/..

for file in $(git ls-files '*.scm' | grep -v prelude); do
  echo '>>>' $file
  snapshot_file=snapshots/${file%.scm}.md
  mkdir -p $(dirname $snapshot_file)
  cat prelude.scm $file | cargo_run stak-compile >main.bc
  cargo_run stak-decode <main.bc >$snapshot_file
done

npx prettier --write snapshots
git diff --exit-code
