#!/bin/sh

set -e

[ -n "$CI" ]

brew install chibi-scheme gauche guile
