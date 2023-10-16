#!/bin/sh

set -ex

document_directory=doc/src/content/docs/examples

cd $(dirname $0)/..

tools/build.sh

export PATH=$PWD/target/release:$PATH

go run github.com/raviqqe/gherkin2markdown features doc/docs/examples

curl -fsSL https://pen-lang.s3.us-west-1.amazonaws.com/icon.svg >doc/docs/icon.svg

(
  cd doc

  npm install
  npm run build

  pip3 install -r requirements.txt

  mkdocs build
)
