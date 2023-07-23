#!/bin/sh

set -e

brew install gambit-scheme

directory=$(dirname $0)/..

gsi $directory/compile.scm $directory/test.scm
