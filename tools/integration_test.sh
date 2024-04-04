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
(cd cmd/mrun && cargo build --release)

export PATH=$PWD/tools/scheme/$interpreter:$PATH

cucumber --publish-quiet --strict-undefined "$@"
