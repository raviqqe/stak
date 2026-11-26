#!/bin/sh

set -ex

. $(dirname $0)/utility.sh

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

prepare_integration_test

export PATH=$PWD/tools/scheme/$interpreter:$PWD/${STAK_TARGET_DIRECTORY:-target}/release_test:$PATH
export STAK_ROOT=$PWD

go tool agoa -t @extra "$@"
