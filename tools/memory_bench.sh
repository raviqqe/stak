#!/bin/sh

set -ex

profile() (
  base=$1
  shift 1

  mkdir -p $(dirname $base)
  out_file=$base.out

  valgrind --tool=massif --massif-out-file=$out_file "$@"
  ms_print $out_file | tee ${out_file%.out}.txt
)

[ $# -eq 0 -a $(uname) = Linux ]

cd $(dirname $0)/..

. tools/utility.sh

brew install valgrind

setup_bench

export PATH=$PWD/target/release:$PWD/cmd/minimal/target/release:$PATH

result_directory=$PWD/tmp/bench/space
mkdir -p $result_directory

cd bench/src

for file in $(ls */main.scm | sort); do
  base=${file%.scm}

  for interpreter in stak mstak chibi-scheme gosh guile; do
    profile $result_directory/$(dirname $base)/$interpreter $interpreter $file
  done
done
