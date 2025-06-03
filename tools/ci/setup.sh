#!/bin/sh

set -e

brew install lua@5.4 pkgconf uutils-coreutils uutils-findutils
cargo install stak

echo LD_LIBRARY_PATH=$(brew --prefix lua@5.4)/lib:$LD_LIBRARY_PATH >>$GITHUB_ENV

git clone https://gitlab.com/jobol/tr7
(
  cd tr7
  mkdir -p /usr/local/share
  make install
)
