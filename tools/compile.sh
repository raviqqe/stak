#!/bin/sh

set -e

scheme=${SCHEME_INTERPRETER:-gsi}
directory=$(dirname $0)/..

cat $directory/prelude.scm "$@" | $scheme $directory/compile.scm
