#!/bin/sh

set -e

directory=$(dirname $0)/..

gsi $directory/compile.scm $directory/main.scm
