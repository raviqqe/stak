#!/bin/sh

set -e

cd $(dirname $0)/..

for file in $(git ls-files '*Cargo.lock'); do
  echo $(dirname $file)
done
