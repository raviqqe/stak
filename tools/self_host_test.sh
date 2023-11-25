#!/bin/sh

set -e

run_stage1() {
  gosh ./compile.scm
}

run_stage2() {
  stak stage2.out
}

run_stage3() {
  stak stage3.out
}

artifact_path() {
  echo tmp/stage$1.$2
}

diff_artifacts() {
  for extension in txt out; do
    diff $(artifact_path $1 $extension) $(artifact_path $2 $extension)
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
    out_file=$(artifact_path $stage out)

    run_stage$stage <$file >$out_file
    stak-decode $out_file >${out_file%.*}.txt
  done

  diff_artifacts 1 2
  diff_artifacts 2 3
done
