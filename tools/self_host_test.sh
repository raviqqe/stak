#!/bin/sh

set -e

stage_count=3

. $(dirname $0)/utility.sh

profile=release_test
target=target/$profile

run_stage() (
  if [ $1 -eq 0 ]; then
    log stak compile.scm
  else
    log $target/stak-interpret stage$1.bc
  fi
)

cd $(dirname $0)/..

directory=tmp/self_host

mkdir -p $directory/compile
cargo build --profile $profile

for stage in $(seq 0 $(expr $stage_count - 1)); do
  cat prelude.scm compile.scm | run_stage $stage >$directory/compile/stage$(expr $stage + 1).bc
done

compile() (
  file=$1

  echo FILE $file

  directory=$directory/${file%.*}

  mkdir -p $directory

  artifact_path() (
    echo $directory/stage$1.$2
  )

  for stage in $(seq 0 $stage_count); do
    bytecode_file=$(artifact_path $stage bc)

    cat prelude.scm $file | run_stage $stage >$bytecode_file
  done

  for stage in $(seq 0 $(expr $stage_count - 1)); do
    for extension in bc; do
      log diff $(artifact_path $stage $extension) $(artifact_path $(expr $stage + 1) $extension)
    done
  done
)

list_scheme_files | parallel compile
