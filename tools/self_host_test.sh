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
  stak-interpret stage2.bc
}

run_stage3() {
  stak-interpret stage3.bc
}

artifact_path() {
  echo tmp/stage$1.$2
}

cd $(dirname $0)/..

export PATH=$(dirname $0)/../target/integration_test:$PATH

mkdir -p tmp
brew install gauche
cargo build --profile integration_test

for stage in $(seq $(expr $stage_count - 1)); do
  cat prelude.scm compile.scm | run_stage$stage >stage$(expr $stage + 1).bc
done

for file in bench/*/main.scm compile.scm; do
  echo '>>>' $file

  for stage in $(seq $stage_count); do
    bytecode_file=$(artifact_path $stage bc)

    cat prelude.scm $file | log run_stage$stage >$bytecode_file
    stak-decode <$bytecode_file >${bytecode_file%.*}.md
  done

  for stage in $(seq $(expr $stage_count - 1)); do
    for extension in md bc; do
      log diff $(artifact_path $stage $extension) $(artifact_path $(expr $stage + 1) $extension)
    done
  done
done
