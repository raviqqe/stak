#!/bin/sh

set -ex

. $(dirname $0)/utility.sh

features=,
interpreter=stak
tags=~@extra

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

prepare_integration_test --features $features

export PATH=$PWD/tools/scheme/$interpreter:$PATH

go tool agoa -t "~@extra && $tags" "$@"
