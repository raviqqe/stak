#!/bin/sh

set -e

brew install --overwrite pkgconf
brew install lua@5.4 uutils-coreutils uutils-findutils
cargo install stak
