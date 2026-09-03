#!/bin/sh

set -e

[ -n "$CI" ]

lua_version=5.5

brew install lua@$lua_version pkgconf
cargo install --locked stak

# Download Go commands by running it.
# https://github.com/actions/setup-go/issues/358
for command in agoa gherkin-format gherkin2markdown; do
  go tool $command --version
done

echo LD_LIBRARY_PATH=$(brew --prefix lua@$lua_version)/lib:$LD_LIBRARY_PATH >>$GITHUB_ENV
