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

env -i \
  PATH=$PWD/tools/scheme/$interpreter:$PATH \
  STAK_ROOT=$PWD \
  $(env | grep STAK_) \
  cucumber --publish-quiet --strict-undefined "$@"
