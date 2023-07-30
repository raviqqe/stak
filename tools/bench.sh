#!/bin/sh

set -e

cargo install hyperfine

for file in $(find -t f bench); do
  target_file=${file%.scm}.out

  tools/compile.sh <$file >$target_file
  hyperfine "stak $target_file"
done
