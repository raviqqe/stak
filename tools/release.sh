#!/bin/sh

set -e

dry_run=false

while getopts d option; do
  case $option in
  d)
    dry_run=true
    ;;
  esac
done

shift $(expr $OPTIND - 1)

for path in $(git ls-files '**/Cargo.toml'); do
  (
    cd $(dirname $path)

    if $dry_run; then
      reference=origin/main
      cargo_options=--dry-run
    else
      reference=HEAD^
    fi

    if git diff $reference -- Cargo.toml | grep '^-version = '; then
      cargo publish $cargo_options
    fi
  )
done
