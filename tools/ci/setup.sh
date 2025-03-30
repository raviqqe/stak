#!/bin/sh

set -e

[ -n "$CI" ]

if [ $(uname) = Linux ]; then
  sudo apt install chibi-scheme guile-3.0 lua-5.4
  curl -fsSL https://raw.githubusercontent.com/practical-scheme/get-gauche/master/get-gauche.sh | sh
else
  brew install chibi-scheme gauche guile lua@5.4 pkgconf uutils-coreutils uutils-findutils
fi

cargo install stak

echo LD_LIBRARY_PATH=$(brew --prefix lua@5.4)/lib:$LD_LIBRARY_PATH >>$GITHUB_ENV
