#!/bin/sh

set -e

if [ $# -ne 0 ]; then
  exit 1
fi

cargo install cargo-workspaces

bytecode_file=compiler/src/compile.bc

cat prelude.scm compile.scm | stak compile.scm >$bytecode_file
git add -f $bytecode_file
git commit -m "Update bytecode file"

cargo workspaces publish -y --from-git
