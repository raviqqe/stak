#!/bin/sh

set -ex

interpreter=stak

while getopts i: option; do
  case $option in
  i)
    interpreter=$OPTARG
    ;;
  esac
done

shift $(expr $OPTIND - 1)

cd $(dirname $0)/..

cargo build --profile release_test
(
  cd cmd/minimal
  cargo build --release
)

export PATH=$PWD/tools/scheme/$interpreter:$PWD/target/release_test:$PATH
export STAK_ROOT=$PWD

go tool agoa -t @extra "$@"
