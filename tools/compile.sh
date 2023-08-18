#!/bin/sh

set -e

scheme=$SCHEME_INTERPRETER
directory=$(dirname $0)/..

$scheme $directory/compile.scm "$@"
