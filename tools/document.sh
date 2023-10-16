#!/bin/sh

set -ex

cd $(dirname $0)/..

directory=doc/src/content/docs/examples

rm -rf $directory/*
go run github.com/raviqqe/gherkin2markdown@latest features $directory

cd doc

npm install
npm run build
