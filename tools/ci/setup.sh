#!/bin/sh

set -e

[ -n "$CI" ]

brew install lua@5.4 pkgconf uutils-coreutils uutils-findutils
cargo install stak

for name in core find; do
  # spell-checker: disable-next-line
  export PATH=$(brew --prefix uutils-${name}utils)/libexec/uubin:$PATH
done

# Download Go commands by running it.
# https://github.com/actions/setup-go/issues/358
for command in agoa gherkin-format gherkin2markdown muffet; do
  go tool $command --version
done

echo LD_LIBRARY_PATH=$(brew --prefix lua@5.4)/lib:$LD_LIBRARY_PATH >>$GITHUB_ENV
