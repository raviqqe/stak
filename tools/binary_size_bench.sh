#!/bin/sh

set -e

build_chibi() (
  cd tmp
  git clone https://github.com/ashinn/chibi-scheme
  cd chibi-scheme
  make chibi-scheme-static
)

build_stak() (
  cargo build --release

  (
    cd cmd/minimal
    cargo build --release
  )
)

. $(dirname $0)/utility.sh

cd $(dirname $0)/..
mkdir -p tmp

build_chibi
build_stak

ls -l \
  target/release/stak \
  cmd/minimal/target/release/mstak \
  tmp/chibi-scheme/chibi-scheme-static
