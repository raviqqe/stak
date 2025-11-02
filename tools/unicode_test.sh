#!/bin/sh

set -ex

version=17.0.0

fetch_data() {
  curl -fsSL https://raw.githubusercontent.com/unicode-org/unicodetools/main/unicodetools/data/ucd/$version/$1.txt
}

cd $(dirname $0)/..

mkdir -p tmp/unicode

cargo build --profile release_test

export PATH=$PWD/target/release_test:$PATH

for base in CaseFolding PropList UnicodeData; do
  fetch_data $base >tmp/$base.txt
done

scheme=${STAK_HOST:-stak}

for type in alphabetic downcase lone-lower lone-upper numeric space upcase; do
  $scheme compile-unicode.scm $type <tmp/UnicodeData.txt >tmp/$type.scm
done

$scheme compile-unicode.scm fold <tmp/CaseFolding.txt >tmp/fold.scm

for type in alphabetic downcase fold lone-lower lone-upper numeric space upcase; do
  cat >tmp/unicode/main.scm <<EOF
(import (scheme base) (scheme char) (scheme cxr) (scheme write) (stak char))

(write
  (case '$type
    ((space)
      (cddddr (cddddr $type-table)))
    (else
      $type-table)))
EOF

  stak tmp/main.scm >tmp/unicode/$type_out.scm

  diff tmp/unicode/$type-out.scm tmp/unicode/$out.scm
done
