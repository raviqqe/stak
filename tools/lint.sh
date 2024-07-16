#!/bin/sh

set -e

cd $(dirname $0)/..

parallel --verbose "cd {1} && cargo clippy $@ {2}" ::: \
  $(tools/workspaces.sh) ::: \
  -- --all-features
