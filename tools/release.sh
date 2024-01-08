#!/bin/sh

set -e

for path in $(git ls-files '**/Cargo.toml'); do
  directory=$(dirname $path)

  (
    cd $directory

    if git diff main -- Cargo.toml | grep '^-version = '; then
      cargo publish "$@"
    fi
  )
done
