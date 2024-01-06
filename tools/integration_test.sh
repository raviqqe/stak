#!/bin/sh

set -ex

feature_flags=,
interpreter=stak-cli

while getopts f:i: option; do
  case $option in
  f)
    feature_flags=$OPTARG
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
cargo build --profile integration_test --bin stak-interpret --features $feature_flags

export PATH=$PWD/tools/scheme/$interpreter:$PATH

cucumber --publish-quiet --strict-undefined "$@"
