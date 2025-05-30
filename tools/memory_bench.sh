#!/bin/sh

set -ex

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
    out_file=$result_directory/$(dirname $base).out

    valgrind --tool massif --massif-out-file $out_file $interpreter $file
    ms_print $out_file | tee ${out_file%.out}.txt
  done
done
