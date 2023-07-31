#!/bin/sh

set -ex

. $(dirname $0)/utility.sh

brew install chicken-scheme gambit-scheme
bundler install

cargo build --release $(feature_flags)

export PATH=$PWD/target/release:$PATH
export ROOT=$PWD

(
  cd $(dirname $0)/..

  cucumber --publish-quiet --strict-undefined "$@"
)
