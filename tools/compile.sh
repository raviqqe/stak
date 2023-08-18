#!/bin/sh

set -e

scheme=${SCHEME_INTERPRETER:-gsi}
directory=$(dirname $0)/..

$scheme $directory/compile.scm "$@"
