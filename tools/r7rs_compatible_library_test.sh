#!/bin/sh

set -e

directory=$(dirname $0)
library_directory=$directory/r7rs

for file in $(ls $library_directory); do
  for name in $(cat $library_directory/$file | grep -v '^\*$'); do
    if ! grep $name $directory/../prelude.scm >/dev/null; then
      echo missing symbol: $name
    fi
  done
done
