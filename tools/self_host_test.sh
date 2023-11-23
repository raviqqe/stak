#!/bin/sh

set -e

run() {
  tools/compile.sh ./compile.scm >compile.out
  cat $1 | cargo run --bin stak compile.out >main.out
  cargo run --bin stak main.out
}

cd $(dirname $0)/..
set -x

for file in test/*.scm; do
  run $file
done
