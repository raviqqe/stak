#!/bin/sh

set -e

git_clone() (
  if [ ! -d $(basename $1) ]; then
    git clone $1
  fi
)

build_chibi() (
  cd tmp
  git_clone https://github.com/ashinn/chibi-scheme
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

build_tr7() (
  cd tmp
  git_clone https://gitlab.com/jobol/tr7
  cd tr7
  make tr7i
)

. $(dirname $0)/utility.sh

cd $(dirname $0)/..
mkdir -p tmp

build_chibi
build_stak
build_tr7

binaries='cmd/minimal/target/release/mstak target/release/stak tmp/chibi-scheme/chibi-scheme-static tmp/tr7/tr7i'

ls -l $binaries
