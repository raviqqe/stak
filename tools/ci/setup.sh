#!/bin/sh

set -e

[ -n "$CI" ]

if [ $(uname) = Linux ]; then
  sudo apt install chibi-scheme guile-3.0 lua5.4
else
  brew install chibi-scheme gauche guile lua@5.4 pkgconf uutils-coreutils uutils-findutils
fi

cargo install stak

echo LD_LIBRARY_PATH=$(brew --prefix lua@5.4)/lib:$LD_LIBRARY_PATH >>$GITHUB_ENV
