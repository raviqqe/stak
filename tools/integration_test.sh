#!/bin/sh

set -ex

features=,
interpreter=stak-cli

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

brew install gauche
bundler install

cargo build --profile integration_test
cargo build --profile integration_test --bin stak-interpret --features $features

export PATH=$PWD/tools/scheme/$interpreter:$PATH

cucumber --publish-quiet --strict-undefined "$@"
