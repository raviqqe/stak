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

if ! echo $interpreter | grep stak; then
  options='--exclude stak --exclude stak-compile'
fi

cargo build --profile release_test --features $featuresa $options

export PATH=$PWD/tools/scheme/$interpreter:$PATH

cucumber --publish-quiet --strict-undefined "$@"
