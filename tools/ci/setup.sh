#!/bin/sh

set -e

# TODO Install a pre-built `chibi-scheme`.
brew install -s chibi-scheme
brew install gauche guile parallel uutils-coreutils uutils-findutils
cargo install stak
