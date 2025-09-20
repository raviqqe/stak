#!/bin/sh

set -e

brew install lua@5.4 pkgconf uutils-coreutils uutils-findutils
cargo install stak

# Download Go commands by running it.
# https://github.com/actions/setup-go/issues/358
for command in agoa gherkin-format gherkin2markdown muffet; do
  go tool agoa --version
done

echo LD_LIBRARY_PATH=$(brew --prefix lua@5.4)/lib:$LD_LIBRARY_PATH >>$GITHUB_ENV

git clone https://gitlab.com/jobol/tr7 /tmp/tr7
(
  cd /tmp/tr7
  make
  cp tr7i /usr/local/bin
)
