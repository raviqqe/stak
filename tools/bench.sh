#!/bin/sh

set -ex

feature=

while getopts f: option; do
  case $option in
  f)
    feature=$OPTARG
    ;;
  esac
done

shift $(expr $OPTIND - 1)

filter=.

if [ $# -gt 0 ]; then
  filter="$@"
fi

cd $(dirname $0)/..

. tools/utility.sh

setup_bench $feature

export PATH=$PWD/target/release:$PWD/cmd/minimal/target/release:$PATH

output_directory=$PWD/tmp/bench/time
mkdir -p $output_directory

cd bench/src

for file in $(ls */main.scm | sort | grep $filter); do
  base=${file%.scm}

  scripts="stak $file,mstak $file,stak-interpret $base.bc,mstak-interpret $base.bc,chibi-scheme $file,gosh $file,guile $file"
  reference=

  if [ $(dirname $base) != eval ]; then
    scripts="$scripts,gsi $file"
  fi

  if which tr7i; then
    scripts="$scripts,tr7i $file"
  fi

  if [ -r $base.py ]; then
    reference="python3 $base.py"
    scripts="micropython $base.py,ruby $base.rb,mruby $base.rb,lua $base.lua,$scripts"
  fi

  hyperfine \
    --shell none \
    --warmup 5 \
    --export-markdown $output_directory/$(dirname $base).md \
    --export-json $output_directory/$(dirname $base).json \
    --input ../../compile.scm \
    ${reference:+--reference "$reference"} \
    -L script "$scripts" \
    "{script}"
done
