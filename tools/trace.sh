#!/bin/sh

set -e

if [ $# -ne 1 ]; then
  exit 1
fi

cd $(dirname $0)/..

tools/compile.sh <$1 >main.out
cargo run --features trace -- main.out
