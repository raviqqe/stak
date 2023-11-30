#!/bin/sh

set -e

stage_count=3

log() {
  echo "$@" >&2
  "$@"
}

run_stage1() {
  stak-compile
}

run_stage2() {
  stak-interpret stage2.out
}

run_stage3() {
  stak-interpret stage3.out
}

artifact_path() {
  echo tmp/stage$1.$2
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
    for extension in txt out; do
      log diff $(artifact_path $stage $extension) $(artifact_path $(expr $stage + 1) $extension)
    done
  done
done
