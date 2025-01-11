#!/bin/sh

set -e

brew install --overwrite pkgconf
brew install uutils-coreutils uutils-findutils
cargo install stak
