#!/bin/sh

set -ex

interpreter=stak-interpret

while getopts i: option; do
  case $option in
  i)
    interpreter=$OPTARG
    ;;
  esac
done

shift $(expr $OPTIND - 1)

if [ $# -ne 0 ]; then
  exit 1
fi

brew install chibi-scheme gambit-scheme gauche

cargo install hyperfine

cd $(dirname $0)/..

for directory in . cmd/minimal; do
  (
    cd $directory
    cargo build --release $@
  )
done

export PATH=$PWD/target/release:$PWD/cmd/minimal/target/release:$PATH

filter=.

if [ $# -gt 0 ]; then
  filter="$@"
fi

for file in $(find bench -type f -name '*.scm' | sort | grep $filter); do
  base=${file%.scm}

  cat prelude.scm $file | stak-compile >$base.bc

  scripts="$interpreter $base.bc,gsi $file,chibi-scheme $file,gosh $file"

  if [ -r $base.py ]; then
    scripts="$scripts,python3 $base.py"
  fi

  hyperfine --sort command --input compile.scm -L script "$scripts" "{script}"
done
