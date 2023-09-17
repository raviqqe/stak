#!/bin/sh

set -ex

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

brew install gambit-scheme gauche
bundler install

cargo build $(feature_flags)

export PATH=$PWD/tools/scheme/${SCHEME_INTERPRETER:-stak}:$PATH

cucumber --publish-quiet --strict-undefined "$@"
