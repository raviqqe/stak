#!/bin/sh

set -e

directory=$(dirname $0)/../../..
raw_arguments="$@"

arguments() {
  echo $raw_arguments | tr ' ' '\n' | grep "$@" '\.scm$'
}

export PATH=$directory/target/release_test:$directory/cmd/minimal/target/release:$PATH

cat $directory/prelude.scm $(arguments) | stak-compile >main.bc
mstak-interpret main.bc $(arguments -v)
