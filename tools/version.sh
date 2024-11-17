#!/bin/sh

set -e

type=${1:-patch}

shift 1

options="$@"

cd $(dirname $0)/..

for directory in . cmd/minimal; do
  (
    cd $directory
    cargo release version $type --execute --no-confirm --allow-branch '*' $options
  )
done
