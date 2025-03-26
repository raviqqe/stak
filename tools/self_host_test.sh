#!/bin/sh

set -e

stage_count=3

. $(dirname $0)/utility.sh

run_stage() {
  if [ $1 -eq 0 ]; then
    log stak compile.scm
  else
    log $target/stak-interpret stage$1.bc
  fi
}

artifact_path() {
  echo tmp/stage$1.$2
}

cd $(dirname $0)/..

profile=release_test
target=target/$profile

mkdir -p tmp
cargo build --profile $profile

for stage in $(seq 0 $(expr $stage_count - 1)); do
  cat prelude.scm compile.scm | run_stage $stage >stage$(expr $stage + 1).bc
done

for file in $(list_scheme_files); do
  echo '>>>' $file

  for stage in $(seq 0 $stage_count); do
    bytecode_file=$(artifact_path $stage bc)

    cat prelude.scm $file | run_stage $stage >$bytecode_file
  done

  for stage in $(seq 0 $(expr $stage_count - 1)); do
    for extension in bc; do
      log diff $(artifact_path $stage $extension) $(artifact_path $(expr $stage + 1) $extension)
    done
  done
done
