#!/bin/sh

set -e

list_dynamic_libraries() {
  case $(uname) in
  Darwin)
    otool -L "$@"
    ;;
  *)
    ldd "$@"
    ;;
  esac
}

git_clone() (
  directory=$(basename $1)

  if [ -d $directory ]; then
    cd $directory
    git pull
  else
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
  if [ $(uname) = Linux ]; then
    target=x86_64-unknown-linux-musl

    rustup target add $target
    options="--target $target"
  fi

  build() (
    cd $1
    cargo build --release $options $2
  )

  build . stak
  build cmd/minimal mstak
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

strip $binaries
ls -l $binaries

for binary in $binaries; do
  list_dynamic_libraries $binary
done
