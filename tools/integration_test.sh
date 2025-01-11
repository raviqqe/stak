#!/bin/sh

set -ex

epoch() {
  date +%s
}

features=,
interpreter=stak

while getopts f:i:t: option; do
  case $option in
  f)
    features=$OPTARG
    ;;
  i)
    interpreter=$OPTARG
    ;;
  t)
    tags=$OPTARG
    ;;
  esac
done

shift $(expr $OPTIND - 1)

cd $(dirname $0)/..

bundler install

brew install chibi-scheme gauche guile
cargo build --profile release_test --features $features
(
  cd cmd/minimal
  cargo build --release
)

export STAK_ROOT=$PWD
export PATH=$PWD/tools/scheme/$interpreter:$PATH

start=$(epoch)
bundler exec cucumber --publish-quiet --strict-undefined ${tags:+-t "$tags"} "$@"
echo Duration: $(expr $(epoch) - $start)s
