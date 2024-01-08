#!/bin/sh

set -e

for path in $(git ls-files '**/Cargo.toml'); do
  directory=$(dirname $path)

  (
    cd $directory
    cargo publish "$@" || :
  )
done
