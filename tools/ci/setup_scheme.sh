#!/bin/sh

set -e

[ -n "$CI" ]

brew install chibi-scheme chicken gauche guile gambit-scheme mit-scheme

git clone https://gitlab.com/jobol/tr7 /tmp/tr7
(
  cd /tmp/tr7
  make
  cp tr7i /usr/local/bin
)
