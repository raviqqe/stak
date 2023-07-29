#!/bin/sh

set -e

cd $(dirname $0)/..

brew install gambit-scheme
bundler install
cargo build --release ${GC_ALWAYS:+--features gc_always}

export PATH=$PWD/target/release:$PATH
export ROOT=$PWD

cucumber --publish-quiet --strict-undefined "$@"
