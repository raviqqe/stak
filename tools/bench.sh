#!/bin/sh

set -ex

cargo install hyperfine

for file in $(find bench -type f -name '*.scm'); do
  target_file=${file%.scm}.out

  cat prelude.scm $file | tools/compile.sh >$target_file
  hyperfine "target/release/stak $target_file"
done
