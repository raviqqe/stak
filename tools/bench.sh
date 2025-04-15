#!/bin/sh

set -ex

features=

while getopts bi option; do
  features="$option$features"
done

shift $(expr $OPTIND - 1)

filter=.

if [ $# -gt 0 ]; then
  filter="$@"
fi

cd $(dirname $0)/..

. tools/utility.sh

setup_bench $features

cd bench/src

for file in $(ls */main.scm | sort | grep $filter); do
  base=${file%.scm}

  scripts="stak $file,mstak $file,stak-interpret $base.bc,mstak-interpret $base.bc,chibi-scheme $file,gosh $file,guile $file"
  reference=

  if [ -r $base.py ]; then
    reference="python3 $base.py"
  fi

  hyperfine \
    -N \
    --input ../../compile.scm \
    ${reference:+--reference "$reference"} \
    -L script "$scripts" \
    "{script}"
done
