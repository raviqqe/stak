#!/bin/sh

set -e

cargo install cargo-workspaces

cat prelude.scm compile.scm | stak compile.scm >compiler/src/compile.bc
cargo workspaces publish -y "$@"
