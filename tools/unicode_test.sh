#!/bin/sh

set -ex

version=final-17.0-20250910

fetch_data() {
  curl -fsSL https://raw.githubusercontent.com/unicode-org/unicodetools/$version/unicodetools/data/ucd/dev/$1.txt
}

cd $(dirname $0)/..

mkdir -p tmp

cargo build --profile release_test

export PATH=$PWD/target/release_test:$PATH

for base in CaseFolding PropList UnicodeData; do
  fetch_data $base >tmp/$base.txt
done

scheme=${STAK_HOST:-stak}

for type in downcase lone-lower lone-upper space upcase; do
  $scheme compile-unicode.scm $type <tmp/UnicodeData.txt >tmp/$type.scm
done

$scheme compile-unicode.scm fold <tmp/CaseFolding.txt >tmp/fold.scm

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
