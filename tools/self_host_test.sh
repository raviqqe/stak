#!/bin/sh

set -e

stage_count=3

log() {
  echo "$@" >&2
  "$@"
}

run_stage1() {
  stak-sac
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
cargo build --release

for stage in $(seq $(expr $stage_count - 1)); do
  cat prelude.scm compile.scm | run_stage$stage >stage$(expr $stage + 1).out
done

for file in test/self_host/*.scm; do
  echo '>>>' $file

  for stage in $(seq $stage_count); do
    out_file=$(artifact_path $stage out)

    log run_stage$stage <$file >$out_file
    stak-decode <$out_file >${out_file%.*}.txt
  done

  for stage in $(seq $(expr $stage_count - 1)); do
    log diff_artifacts $stage $(expr $stage + 1)
  done
done
