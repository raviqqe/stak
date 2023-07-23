#!/bin/sh

set -e

if [ $# -ne 0 ]; then
  exit 1
fi

directory=$(dirname $0)/..

gsi $directory/compile.scm $directory/main.scm
