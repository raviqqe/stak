#!/bin/sh

set -ex

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

brew install gambit-scheme gauche
bundler install

cargo build $(feature_flags)

interpreter=${SCHEME_INTERPRETER:-stak}

export PATH=$PWD/tools/scheme/$interpreter:$PATH

cucumber \
  --publish-quiet \
  --strict-undefined \
  --tags "not $([ $interpreter = stak ] && echo @any || echo @nonstandard)" \
  "$@"
