#!/bin/sh

set -e

while getopts t: option; do
  case $option in
  t)
    options="--tags '$OPTARG'"
    ;;
  esac
done

shift $(expr $OPTIND - 1)

file=$1

cd $(dirname $0)/..

directory=tmp/$(basename ${file%.*})

mkdir -p $directory
cd $directory

output=$(bundler exec cucumber --publish-quiet --strict-undefined --require ../../features $options ../../$file)
status=$?

echo "$output"

exit $status
