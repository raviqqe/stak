#!/bin/sh

set -ex

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

brew install gauche
bundler install

if [ -z "$STAK_DEBUG" ]; then
  flags=---release
fi

cargo build $flags
cargo build $flags --bin stak-interpret $(feature_flags)

export PATH=$PWD/tools/scheme/${SCHEME_INTERPRETER:-stak}:$PATH

cucumber --publish-quiet --strict-undefined "$@"
