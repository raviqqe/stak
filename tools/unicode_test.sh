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

for type in downcase lone-lower lone-upper space upcase; do
  stak compile-unicode.scm $type <tmp/UnicodeData.txt >tmp/$type.scm
done

stak compile-unicode.scm fold <tmp/CaseFolding.txt >tmp/fold.scm

for type in downcase fold lone-lower lone-upper space upcase; do
  cat >tmp/main.scm <<EOF
(import (scheme base) (scheme char) (scheme cxr) (scheme write) (stak char))

(write
  (case '$type
    ((space)
      (cddddr (cddddr $type-table)))
    (else
      $type-table)))
EOF

  [ "$(stak tmp/main.scm)" = "$(cat tmp/$type.scm)" ]
done
