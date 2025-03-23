#!/bin/sh

set -e

cd $(dirname $0)/..

. $(dirname $0)/utility.sh

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
done

npx prettier --write snapshots
git diff --exit-code
