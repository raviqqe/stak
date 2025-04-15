#!/bin/sh

set -e

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

brew install chibi-scheme gauche guile
interpreters='chibi-scheme gosh guile'

cargo build --release
export PATH=$PWD/target/release:$PATH

for file in $(list_scheme_files); do
  echo '>>>' $file

  for interpreter in $interpreters stak; do
    cat prelude.scm $file | log $interpreter compile.scm >$interpreter.bc
  done

  for interpreter in $interpreters; do
    log diff stak.bc $interpreter.bc
  done
done
