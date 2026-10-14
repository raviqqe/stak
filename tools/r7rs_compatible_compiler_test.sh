#!/bin/sh

set -e

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

list_interpreters() (
  for interpreter in chibi-scheme gosh guile; do
    case $interpreter:$1 in
    chibi-scheme:examples/embedded-script/src/fight.scm)
      # TODO Fix a segmentation fault of Chibi Scheme.
      ;;
    *)
      echo $interpreter
      ;;
    esac
  done
)

compile() (
  case $1 in
  gosh)
    # TODO Use vanilla Gauche.
    log gosh -fno-inline-locals compile.scm
    ;;
  *)
    log $1 compile.scm
    ;;
  esac
)

cargo build --release
export PATH=$PWD/target/release:$PATH

for file in $(list_scheme_files); do
  echo FILE $file

  for interpreter in $(list_interpreters $file) stak; do
    cat prelude.scm $file | compile $interpreter >$interpreter.bc
  done

  for interpreter in $(list_interpreters $file); do
    log diff stak.bc $interpreter.bc
  done
done
