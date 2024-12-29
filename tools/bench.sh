#!/bin/sh

set -ex

while getopts d option; do
  case $option in
  d)
    build_options=--no-default-features
    ;;
  esac
done

shift $(expr $OPTIND - 1)

if [ $# -ne 0 ]; then
  exit 1
fi

cd $(dirname $0)/..

. tools/utility.sh
bench
