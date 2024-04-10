#!/bin/sh

set -ex

if [ $# -ne 0 ]; then
  exit 1
fi

cargo install cargo-workspaces

bytecode_file=compiler/src/compile.bc

cat prelude.scm compile.scm | stak compile.scm >$bytecode_file

git config user.email action@github.com
git config user.name 'GitHub Action'
git add -f $bytecode_file
git commit -m "Update bytecode file"

for directory in . cmd/minimal; do
  (
    cd $directory
    cargo workspaces publish -y --from-git
  )
done
