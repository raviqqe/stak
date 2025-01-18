#!/bin/sh

set -e

if which brew; then
  brew install --overwrite pkgconf
  brew install uutils-coreutils uutils-findutils
fi

cargo install stak
