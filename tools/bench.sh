#!/bin/sh

set -e

cargo install hyperfine

for file in $(ls bench); do
  hyperfine stak
done
