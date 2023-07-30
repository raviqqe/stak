#!/bin/sh

set -e

cargo install hyperfine

for file in $(find -t f bench); do
  hyperfine stak ${file}
done
