#!/bin/sh

set -e

brew install --overwrite pkgconf
brew install chibi-scheme gauche guile uutils-coreutils uutils-findutils
cargo install stak
