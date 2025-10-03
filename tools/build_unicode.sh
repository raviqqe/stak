#!/bin/sh

set -ex

cd $(dirname $0)/..

mkdir -p tmp

cargo build --profile release_test

export PATH=$PWD/target/release_test:$PATH

curl -fsSL https://raw.githubusercontent.com/unicode-org/icu/24ebdccde39af10d8564fb7d50ad7f11a26e86b9/icu4c/source/data/unidata/CaseFolding.txt >tmp/folding.txt
gosh parse-folding.scm <tmp/folding.txt >tmp/folding.scm
