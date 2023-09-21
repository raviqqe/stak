#!/bin/sh

set -e

directory=$(dirname $0)/..

cat $directory/prelude.scm "$@" | ${SCHEME_HOST_INTERPRETER:-gsi} $directory/compile.scm
