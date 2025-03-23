#!/bin/sh

set -e

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

interpreters='chibi-scheme gauche guile'
brew install $interpreters

cargo build --release
export PATH=$PWD/target/release:$PATH

for file in $(list_scheme_files); do
  echo '>>>' $file

  for interpreter in $interpreters stak; do
    echo '>>>>>>' $interpreter
    $interpreter compile.scm <prelude.scm <$file >$interpreter.bc
  done

  for interpreter in $interpreters; do
    diff stak.bc $interpreter.bc
  done
done

npx prettier --write snapshots
git diff --exit-code
