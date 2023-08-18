#!/bin/sh

set -ex

. $(dirname $0)/utility.sh

brew install gambit-scheme gauche
bundler install

cargo build $(feature_flags)

export PATH=$PWD/target/debug:$PWD/tools:$PATH
export ROOT=$PWD

(
  cd $(dirname $0)/..

  cucumber --publish-quiet --strict-undefined "$@"
)
