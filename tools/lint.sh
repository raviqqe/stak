#!/bin/sh

set -e

parallel --verbose "cd {1} && cargo clippy $@" ::: $($(dirname $0)/workspaces.sh)
