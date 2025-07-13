#!/bin/sh

set -e

list_dynamic_libraries() {
  case $(uname) in
  Darwin)
    otool -L "$@" | tail -n +2 | grep -o '.*\.dylib'
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
  for directory in . cmd/minimal; do
    (
      cd $directory
      cargo build --release
    )
  done
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

for binary in $binaries; do
  echo '>>>' $binary
  ls -l $binary $(list_dynamic_libraries $binary)
done
