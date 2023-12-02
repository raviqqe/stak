#!/bin/sh

set -ex

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

brew install gauche
bundler install

flags=${STAK_DEBUG:+--release}

cargo build $flags
cargo build $flags --bin stak-interpret $(feature_flags)

export PATH=$PWD/tools/scheme/${SCHEME_INTERPRETER:-stak}:$PATH

cucumber --publish-quiet --strict-undefined "$@"
