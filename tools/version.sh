#!/bin/sh

set -e

cd $(dirname $0)/..

for directory in . cmd/minimal; do
  (
    cd $directory
    cargo release version patch --execute --no-confirm --allow-branch '*'
  )
done
