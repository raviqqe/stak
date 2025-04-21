#!/bin/sh

set -ex

while getopts l option; do
  case $option in
  l)
    localhost=true
    ;;
  esac
done

shift $(expr $OPTIND - 1)

[ $# -eq 0 ]

cd $(dirname $0)/..

cargo build --release --target wasm32-wasip1 --bin stak-repl
cp target/wasm32-wasip2/release/stak-repl.wasm doc/public

directory=doc/src/content/docs/examples

rm -rf $directory/*
go run github.com/raviqqe/gherkin2markdown@latest features $directory

rm $(find $directory -name '*smoke*')

for file in $(find $directory -name '*.md'); do
  new_file=$(dirname $file)/$(basename $file).tmp

  (
    echo ---
    echo title: $(grep -o '^# \(.*\)$' $file | sed 's/# *//')
    echo ---
    cat $file | grep -v '^# '
  ) >$new_file
  mv $new_file $file
done

cd doc

npm ci
npm run build -- ${localhost:+--site http://localhost:4321/stak}
