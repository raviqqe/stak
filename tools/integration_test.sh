#!/bin/sh

set -ex

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

brew install gambit-scheme gauche
bundler install

cargo build $(feature_flags)

export PATH=$PWD/target/debug:$PWD/tools:$PATH
export STAK_ROOT=$PWD

cucumber --publish-quiet --strict-undefined "$@"
