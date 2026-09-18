#!/bin/sh

set -e

[ -n "$CI" ]

brew install chibi-scheme chicken gauche guile
chicken-install srfi-1
