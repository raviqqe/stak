#!/bin/sh

set -ex

version=release-77-1

fetch_data() {
  curl -fsSL https://raw.githubusercontent.com/unicode-org/icu/$version/icu4c/source/data/unidata/$1.txt
}

cd $(dirname $0)/..

mkdir -p tmp

cargo build --profile release_test

export PATH=$PWD/target/release_test:$PATH

for base in CaseFolding UnicodeData; do
  fetch_data $base >tmp/$base.txt
done

stak parse-unicode.scm fold <tmp/CaseFolding.txt >tmp/fold.scm
stak parse-unicode.scm case <tmp/UnicodeData.txt >tmp/case.scm

[ "$(stak tools/unicode/fold.scm)" = "$(cat tmp/fold.scm)" ]
