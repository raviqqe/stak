#!/bin/sh

set -e

type=${1:-patch}

if [ $# -gt 0 ]; then
  shift 1
fi

options="$@"

cd $(dirname $0)/..

for directory in . cmd/minimal; do
  (
    cd $directory
    cargo release version $type --execute --no-confirm --allow-branch '*' $options
  )
done
