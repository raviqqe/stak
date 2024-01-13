#!/bin/sh

set -e

if [ $# -ne 0 ]; then
  exit 1
fi

cargo install cargo-workspaces

cat prelude.scm compile.scm | stak compile.scm >compiler/src/compile.bc
cargo workspaces publish -y --from-git
