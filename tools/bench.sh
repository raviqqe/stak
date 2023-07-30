#!/bin/sh

set -ex

cargo install hyperfine

for file in $(find bench -type f '*.scm'); do
  target_file=${file%.scm}.out

  tools/compile.sh <$file >$target_file
  hyperfine "target/release/stak $target_file"
done
