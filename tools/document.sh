#!/bin/sh

set -ex

cd $(dirname $0)/..

directory=doc/src/content/docs/examples

brew install gauche

rm -rf $directory/*
go run github.com/raviqqe/gherkin2markdown@latest features $directory

rm $(find $directory -name '*smoke*')

for file in $(find $directory -name '*.md'); do
  new_file=$(dirname $file)/new_$(basename $file)

  (
    echo ---
    echo title: $(grep -o '^# \(.*\)$' $file | sed 's/# *//')
    echo ---
    cat $file | grep -v '^# '
  ) >$new_file
  mv $new_file $file
done

cd doc

cargo doc --no-deps --all-features
npm install
npm run build
