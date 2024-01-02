#!/bin/sh

set -ex

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

brew install gauche
bundler install

cargo build --profile integration_test
cargo build --profile integration_test --bin stak-interpret $(feature_flags)

export PATH=$PWD/tools/scheme/${SCHEME_INTERPRETER:-stak}:$PATH

cucumber --publish-quiet --strict-undefined "$@"
