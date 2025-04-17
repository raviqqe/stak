#!/bin/sh

set -ex

build_binary() {
  (
    cd $1
    shift 1
    cargo build --release
    cargo build --release "$@"
  )
}

setup() {
  [ $# -le 1 ]

  feature=$1

  brew install chibi-scheme gambit-scheme gauche guile micropython
  cargo install hyperfine

  case $feature in
  i63)
    build_options='--no-default-features --features std'
    ;;
  f62)
    build_options='--no-default-features --features std,float62'
    ;;
  esac

  build_binary . -p stak -p stak-interpret $build_options
  build_binary cmd/minimal -p mstak -p mstak-interpret

  export PATH=$PWD/target/release:$PWD/cmd/minimal/target/release:$PATH

  for file in bench/src/*/main.scm; do
    cat prelude.scm $file | stak-compile >${file%.scm}.bc
  done
}

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

setup_bench $features

cd bench/src

for file in $(ls */main.scm | sort | grep $filter); do
  base=${file%.scm}

  scripts="stak $file,mstak $file,stak-interpret $base.bc,mstak-interpret $base.bc,chibi-scheme $file,gosh $file,guile $file"
  reference=

  if [ -r $base.py ]; then
    reference="python3 $base.py"
    scripts="micropython $base.py,$scripts"
  fi

  hyperfine \
    -N \
    --input ../../compile.scm \
    ${reference:+--reference "$reference"} \
    -L script "$scripts" \
    "{script}"
done
