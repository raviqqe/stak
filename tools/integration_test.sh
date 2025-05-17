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

brew install chibi-scheme gauche guile parallel
bundler install

cargo build --profile release_test --features $features
(
  cd cmd/minimal
  cargo build --release
)

export STAK_ROOT=$PWD
export PATH=$PWD/tools/scheme/$interpreter:$PATH

start=$(epoch)

if [ $# -eq 0 ]; then
  git ls-files '**/*.feature' | xargs ls -S | parallel -q tools/cucumber.sh ${tags:+-t "$tags"}
else
  bundler exec cucumber --publish-quiet --strict-undefined "$@"
fi

echo Duration: $(expr $(epoch) - $start)s
