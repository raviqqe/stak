#!/bin/sh

set -e

cargo install cargo-release

cat prelude.scm compile.scm | stak compile.scm >compiler/src/compile.bc
cargo release publish --no-confirm "$@"
