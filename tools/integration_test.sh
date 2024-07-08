#!/bin/sh

set -ex

features=,
interpreter=stak

while getopts f:i:t: option; do
  case $option in
  f)
    features=$OPTARG
    ;;
  i)
    interpreter=$OPTARG
    ;;
  t)
    tags=$OPTARG
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

if [ -n "$tags" ]; then
  options="--tags '$tags'"
fi

if [ $# -eq 0 ]; then
  git ls-files '**/*.feature' | xargs -P $(nproc) -I % tools/cucumber.sh $options %
else
  $cucumber "$@"
fi
