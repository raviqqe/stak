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

stak compile-unicode.scm case <tmp/UnicodeData.txt >tmp/case.scm
stak compile-unicode.scm fold <tmp/CaseFolding.txt >tmp/fold.scm

for type in case fold; do
  cat >tmp/main.scm <<EOF
(import (stak char) (scheme write))

(write $type-table)
EOF

  [ "$(stak tmp/main.scm)" = "$(cat tmp/$type.scm)" ]
done
