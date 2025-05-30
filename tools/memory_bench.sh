#!/bin/sh

set -ex

[ $# -eq 0 ]

cd $(dirname $0)/..

. tools/utility.sh

setup_bench

result_directory=$PWD/tmp/bench/space
mkdir -p $result_directory

cd bench/src

for file in $(ls */main.scm | sort); do
  base=${file%.scm}

  for interpreter in stak mstak chibi-scheme gosh guile; do
    /usr/bin/time -v $interpreter $file
  done
done
