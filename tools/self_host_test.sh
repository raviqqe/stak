#!/bin/sh

set -e

run() {
  tools/compile.sh ./compile.scm >compile.out
  cargo run --release --bin stak compile.out <$1 >main.out
  cargo run --release --bin stak main.out
}

cd $(dirname $0)/..

brew install gauche

set -x

for file in test/*.scm; do
  run $file
done
