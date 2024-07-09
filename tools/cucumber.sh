#!/bin/sh

set -ex

while getopts t: option; do
  case $option in
  t)
    tags=$OPTARG
    ;;
  esac
done

shift $(expr $OPTIND - 1)

if [ $# -ne 1 ]; then
  exit 1
fi

file=$1

cd $(dirname $0)/..

directory=tmp/$(basename ${file%.*})

mkdir -p $directory
cd $directory

output=$(bundler exec cucumber --publish-quiet --strict-undefined --require ../../features ${tags:+--tags "$tags"} ../../$file 2>£1)
status=$?

echo "$output"

exit $status
