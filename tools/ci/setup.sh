#!/bin/sh

set -e

if which brew; then
  brew install uutils-coreutils uutils-findutils
fi

cargo install stak
