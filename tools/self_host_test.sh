#!/bin/sh

set -e

directory=$(dirname $0)/..

alias stak=$directory/target/release/stak
alias decode=$directory/target/release/stak-decode

run() {
  gosh ./compile.scm <$1 >$2
  decode $2 >$3
}

run_self() {
  stak compile.out <$1 >$2
  decode $2 >$3
}

cd $(dirname $0)/..

mkdir -p tmp
brew install gauche
tools/compile.sh ./compile.scm >compile.out

set -x

for file in test/self_host/*.scm; do
  run $file tmp/other.out tmp/other.txt
  run_self $file tmp/self.out tmp/self.txt

  diff tmp/other.txt tmp/self.txt
  diff tmp/other.out tmp/self.out
done
