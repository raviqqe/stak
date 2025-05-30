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

output_directory=$PWD/tmp/bench/space
mkdir -p $output_directory

cd bench/src

for file in $(ls */main.scm | sort | grep -v eval); do
  base=${file%.scm}
  directory=$output_directory/$(dirname $base)

  profile $directory/stak-interpret stak-interpret --heap-size 16384 $base.bc

  for command in chibi-scheme gosh guile; do
    profile $directory/$command $command $file
  done
done
