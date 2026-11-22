#!/bin/sh

set -ex

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

cargo build --profile release_test --features $features
cargo build --profile release_test --target-dir target/libc -p stak --no-default-features --features float,libc
(
  cd cmd/minimal
  cargo build --release
)

export PATH=$PWD/tools/scheme/$interpreter:$PATH

go tool agoa -t "~@extra && $tags" "$@"
