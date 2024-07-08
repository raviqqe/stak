#!/bin/sh

set -ex

features=,
interpreter=stak

while getopts f:i: option; do
  case $option in
  f)
    features=$OPTARG
    ;;
  i)
    interpreter=$OPTARG
    ;;
  esac
done

shift $(expr $OPTIND - 1)

cd $(dirname $0)/..

bundler install

cargo build --profile release_test --features $features
(
  cd cmd/minimal
  cargo build --release
)

export STAK_ROOT=$PWD
export PATH=$PWD/tools/scheme/$interpreter:$PATH

cucumber='cucumber --publish-quiet --strict-undefined'

if [ $# -ne 0 ]; then
  $cucumber "$@"
else
  git ls-files '**/*.feature' |
    xargs -I % sh -c "mkdir -p tmp/% && cd tmp/% && $cucumber ../../%"
fi
