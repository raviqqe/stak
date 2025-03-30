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

cargo build --profile release_test --features $features
(
  cd cmd/minimal
  cargo build --release
)

export STAK_ROOT=$PWD/tools/scheme/$interpreter
export PATH=$STAK_ROOT:$PATH

start=$(epoch)

if [ $# -eq 0 ]; then
  git ls-files '**/*.feature' | xargs ls -S | parallel -q tools/cucumber.sh ${tags:+-t "$tags"}
else
  bundler exec cucumber --publish-quiet --strict-undefined "$@"
fi

echo Duration: $(expr $(epoch) - $start)s
