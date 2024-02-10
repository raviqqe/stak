#!/bin/sh

set -e

stage_count=3

log() {
  echo "$@" >&2
  "$@"
}

run_stage0() {
  stak compile.scm
}

run_stage1() {
  $target/stak-compile
}

run_stage2() {
  $target/stak-interpret stage2.bc
}

run_stage3() {
  $target/stak-interpret stage3.bc
}

artifact_path() {
  echo tmp/stage$1.$2
}

cd $(dirname $0)/..

target=$PWD/target/release_test

mkdir -p tmp
cargo build --profile release_test

for stage in $(seq 0 $(expr $stage_count - 1)); do
  cat prelude.scm compile.scm | run_stage$stage >stage$(expr $stage + 1).bc
done

for file in bench/*/main.scm compile.scm; do
  echo '>>>' $file

  for stage in $(seq 0 $stage_count); do
    bytecode_file=$(artifact_path $stage bc)

    cat prelude.scm $file | log run_stage$stage >$bytecode_file
    $target/stak-decode <$bytecode_file >${bytecode_file%.*}.md
  done

  for stage in $(seq 0 $(expr $stage_count - 1)); do
    for extension in md bc; do
      log diff $(artifact_path $stage $extension) $(artifact_path $(expr $stage + 1) $extension)
    done
  done
done
