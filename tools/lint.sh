#!/bin/sh

set -e

parallel --verbose "cd {} && cargo clippy $@" ::: $($(dirname $0)/workspaces.sh)
