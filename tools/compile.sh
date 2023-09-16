#!/bin/sh

set -e

directory=$(dirname $0)/..

cat $directory/prelude.scm "$@" | gsi $directory/compile.scm
