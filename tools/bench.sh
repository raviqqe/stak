#!/bin/sh

set -ex

interpreter=stak-interpret
filter=.

while getopts fi: option; do
  case $option in
  f)
    filter=$OPTARG
    ;;
  i)
    interpreter=$OPTARG
    ;;
  esac
done

shift $(expr $OPTIND - 1)

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

for file in $(find bench -type f -name '*.scm' | sort | grep $filter); do
  base=${file%.scm}

  cat prelude.scm $file | stak-compile >$base.bc

  scripts="$interpreter $base.bc,gsi $file,chibi-scheme $file,gosh $file"

  if [ -r $base.py ]; then
    scripts="$scripts,python3 $base.py"
  fi

  hyperfine --sort command --input compile.scm -L script "$scripts" "{script}"
done
