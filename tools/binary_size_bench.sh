#!/bin/sh

set -e

. $(dirname $0)/utility.sh

filter_existent_paths() (
  for path in "$@"; do
    if [ -r $path ]; then
      readlink -f $path
    fi
  done
)

list_dynamic_libraries() (
  case $(uname) in
  Darwin)
    # cspell: disable-next-line
    if otool -hv $1 | grep -q DYLDLINK; then
      otool -L $1 | tail -n +2 | grep -o '.*\.dylib'
    fi
    ;;
  *)
    if file $1 | grep -q 'dynamically linked'; then
      ldd $1 | grep -o '/lib/[^ ]*'
    fi
    ;;
  esac
)

build_chibi() (
  git_clone https://github.com/ashinn/chibi-scheme tmp/chibi-scheme

  cd tmp/chibi-scheme

  # spell-checker: disable-next-line
  make CFLAGS='-Os -DSEXP_USE_FLONUMS=1 -DSEXP_USE_NO_FEATURES=1' chibi-scheme-static
)

build_stak() (
  target=$1

  if [ -n "$target" ]; then
    rustup target add $target
    options="--target $target"
  fi

  build() (
    cd $1
    cargo build --release --bin $2 $options
  )

  build . stak
  build cmd/minimal mstak
)

cd $(dirname $0)/..
mkdir -p tmp

if [ $(uname) = Linux ]; then
  target=$(uname -m)-unknown-linux-musl
fi

build_chibi
build_tr7
build_stak $target

binaries="cmd/minimal/target/$target/release/mstak target/$target/release/stak tmp/chibi-scheme/chibi-scheme-static tmp/tr7/tr7i"

strip $binaries

uname -a

for binary in $binaries; do
  libraries=$(list_dynamic_libraries $binary)

  echo $binary '=>' $libraries

  for file in $(filter_existent_paths $libraries); do
    wc -c $file
  done
done

for binary in $binaries; do
  echo $(basename $binary) $(wc -c <$binary)
done | tee tmp/binary_size.txt
