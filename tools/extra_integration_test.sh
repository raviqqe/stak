#!/bin/sh

set -ex

cd $(dirname $0)/..

cargo build --profile release_test

export PATH=$PWD/target/release_test:$PATH
export STAK_ROOT=$PWD

go tool agoa -t @extra "$@"
