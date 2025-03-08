#!/bin/sh

set -e

brew install --overwrite pkgconf
brew install lua@5.4 uutils-coreutils uutils-findutils
cargo install stak

echo LD_LIBRARY_PATH=$(brew --prefix lua@5.4)/lib:$LD_LIBRARY_PATH >>$GITHUB_ENV
