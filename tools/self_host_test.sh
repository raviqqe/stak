#!/bin/sh

set -e

run_stage1() {
  gosh ./compile.scm <$1 >$2
  stak-decode $2 >$3
}

run_stage2() {
  stak stage2.out <$1 >$2
  stak-decode $2 >$3
}

run_stage3() {
  stak stage3.out <$1 >$2
  stak-decode $2 >$3
}

diff_artifacts() {
  for extension in txt out; do
    diff tmp/stage$1.$extension tmp/stage$2.$extension
  done
}

cd $(dirname $0)/..

export PATH=$(dirname $0)/../target/release:$PATH

mkdir -p tmp
brew install gauche

tools/compile.sh ./compile.scm >stage2.out
cat prelude.scm compile.scm | stak stage2.out >stage3.out

set -x

for file in test/self_host/*.scm; do
  for stage in $(seq 3); do
    run_stage$stage $file tmp/stage$stage.out tmp/stage$stage.txt
  done

  diff_artifacts 1 2
  diff_artifacts 2 3
done
