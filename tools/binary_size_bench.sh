#!/bin/sh

set -e

filter_existent_paths() (
  for path in "$@"; do
    if [ -r $path ]; then
      readlink -f $path
    fi
  done
)

list_dynamic_libraries() {
  case $(uname) in
  Darwin)
    otool -L "$@" | tail -n +2 | grep -o '.*\.dylib'
    ;;
  *)
    ldd /bin/ls | grep -o '/lib/[^ ]*'
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

  flags=

  # spell-checker: disable
  for flag in \
    SEXP_USE_AUTOCLOSE_PORTS \
    SEXP_USE_BIGNUMS \
    SEXP_USE_BOEHM \
    SEXP_USE_CHECK_STACK \
    SEXP_USE_DL \
    SEXP_USE_ESCAPE_NEWLINE \
    SEXP_USE_EXTENDED_CHAR_NAMES \
    SEXP_USE_EXTENDED_FCALL \
    SEXP_USE_FOLD_CASE_SYMS \
    SEXP_USE_FULL_SOURCE_INFO \
    SEXP_USE_GLOBAL_HEAP \
    SEXP_USE_GREEN_THREADS \
    SEXP_USE_GROW_STACK \
    SEXP_USE_HASH_SYMS \
    SEXP_USE_HUFF_SYMS \
    SEXP_USE_IMAGE_LOADING \
    SEXP_USE_INFINITIES \
    SEXP_USE_LONG_PROCEDURE_ARGS \
    SEXP_USE_MAIN_ERROR_ADVISE \
    SEXP_USE_MAIN_HELP \
    SEXP_USE_MATH \
    SEXP_USE_MINI_FLOAT_UNIFORM_VECTORS \
    SEXP_USE_MODULES \
    SEXP_USE_NO_FEATURES \
    SEXP_USE_OBJECT_BRACE_LITERALS \
    SEXP_USE_READER_LABELS \
    SEXP_USE_SIMPLIFY \
    SEXP_USE_TAIL_JUMPS \
    SEXP_USE_TYPE_DEFS \
    SEXP_USE_UNBOXED_LOCALS \
    SEXP_USE_UNIFORM_VECTOR_LITERALS \
    SEXP_USE_UTF8_STRINGS \
    SEXP_USE_WARN_UNDEFS \
    SEXP_USE_WEAK_REFERENCES; do
    flags="$flags -D$flag=0"
  done
  # spell-checker: enable

  set -x
  make CFLAGS=-Os CPPFLAGS="$flags" chibi-scheme-static
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

uname -a

for binary in $binaries; do
  libraries=$(list_dynamic_libraries $binary)

  echo $binary '=>' $libraries
  ls -lX $binary $(filter_existent_paths $libraries)
done
