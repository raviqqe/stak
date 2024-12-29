#!/bin/sh

set -ex

while getopts d option; do
  case $option in
  d)
    build_options=--no-default-features
    ;;
  esac
done

shift $(expr $OPTIND - 1)

[ $# -eq 0 ]

cd $(dirname $0)/..

. tools/utility.sh

setup_bench $build_options

filter=.

if [ $# -gt 0 ]; then
  filter="$@"
fi

for file in $(find bench -type f -name '*.scm' | sort | grep $filter); do
  base=${file%.scm}

  scripts="stak $file,mstak $file,stak-interpret $base.bc,mstak-interpret $base.bc,gsi $file,chibi-scheme $file,gosh $file"

  if [ -r $base.py ]; then
    scripts="$scripts,python3 $base.py"
  fi

  hyperfine --sort command --input compile.scm -L script "$scripts" "{script}"
done
