#!/bin/sh

set -e

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

interpreters='chibi-scheme gosh guile'

compile() (
  case $1 in
  gosh)
    # Avoid a VM bug in Gauche.
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

  for interpreter in $interpreters stak; do
    cat prelude.scm $file | compile $interpreter >$interpreter.bc
  done

  for interpreter in $interpreters; do
    log diff stak.bc $interpreter.bc
  done
done
