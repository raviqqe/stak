#!/bin/sh

set -e

directory=$(dirname $0)
library_directory=$directory/r7rs

for file in $(ls $library_directory); do
  echo $(echo ${file%.scm} | sed s/_/-/g)

  for name in $(cat $library_directory/$file | grep -v -e '^\*$' -e include); do
    if ! grep $name $directory/../prelude.scm >/dev/null; then
      echo "  $name"
    fi
  done
done
