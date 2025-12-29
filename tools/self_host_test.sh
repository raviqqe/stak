#!/bin/sh

set -e

stage_count=3

. $(dirname $0)/utility.sh

profile=release_test
target=target/$profile
directory=tmp/self_host

compile() (
  if [ $1 -eq 0 ]; then
    log stak compile.scm
  else
    log $target/stak-interpret $directory/stage$1.bc
  fi
)

cd $(dirname $0)/..

mkdir -p $directory
cargo build --profile $profile

for stage in $(seq 0 $(expr $stage_count - 1)); do
  cat prelude.scm compile.scm | compile $stage >$directory/stage$(expr $stage + 1).bc
done

test_file() (
  file=$1

  echo FILE $file

  sub_directory=$directory/${file%.*}

  mkdir -p $sub_directory

  bytecode_file() (
    echo $sub_directory/stage$1.bc
  )

  for stage in $(seq 0 $stage_count); do
    cat prelude.scm $file | compile $stage >$(bytecode_file $stage)
  done

  for stage in $(seq 0 $(expr $stage_count - 1)); do
    log diff $(bytecode_file $stage) $(bytecode_file $(expr $stage + 1))
  done
)

list_scheme_files | parallel test_file
