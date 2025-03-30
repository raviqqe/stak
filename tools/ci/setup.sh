#!/bin/sh

set -e

if [ $(uname) = Linux ]; then
  apt install chibi-scheme gauche guile lua
else
  brew install chibi-scheme gauche guile lua@5.4 pkgconf uutils-coreutils uutils-findutils
fi

go install github.com/shenwei356/rush@latest
cargo install stak

echo LD_LIBRARY_PATH=$(brew --prefix lua@5.4)/lib:$LD_LIBRARY_PATH >>$GITHUB_ENV
