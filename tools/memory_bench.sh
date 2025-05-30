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

heap_size() (
  bytecode_file=$1
  size=8192

  for _ in $(seq 8); do
    if stak-interpret --heap-size $size $bytecode_file >/dev/null; then
      echo $size
      return
    fi

    size=$(expr $size '*' 2)
  done

  return 1
)

[ $# -eq 0 -a $(uname) = Linux ]

cd $(dirname $0)/..

. tools/utility.sh

brew install valgrind

setup_bench

export PATH=$PWD/target/release:$PWD/cmd/minimal/target/release:$PATH

output_directory=$PWD/tmp/bench/memory
mkdir -p $output_directory

cd bench/src

for file in $(ls */main.scm | sort | grep -v eval); do
  base=${file%.scm}
  directory=$output_directory/$(dirname $base)

  profile $directory/stak-interpret stak-interpret --heap-size $(heap_size $base.bc) $base.bc

  for command in chibi-scheme gosh guile; do
    profile $directory/$command $command $file
  done

  for command in python3 micropython; do
    profile $directory/$command $command $base.py
  done

  for command in ruby mruby; do
    profile $directory/$command $command $base.rb
  done

  profile $directory/lua lua $base.lua
done
