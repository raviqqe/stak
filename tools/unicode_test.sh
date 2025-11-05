#!/bin/sh

set -ex

version=17.0.0

fetch_data() {
  curl -fsSL https://raw.githubusercontent.com/unicode-org/unicodetools/main/unicodetools/data/ucd/$version/$1.txt
}

cd $(dirname $0)/..

directory=tmp/unicode

mkdir -p $directory

cargo build --profile release_test

export PATH=$PWD/target/release_test:$PATH

for base in CaseFolding PropList UnicodeData; do
  fetch_data $base >$directory/$base.txt
done

alias scheme=${STAK_HOST:-stak}

for type in downcase lone-lower lone-upper numeric space upcase; do
  $scheme compile-unicode.scm $type <$directory/UnicodeData.txt >$directory/$type.scm
done

scheme compile-unicode.scm alphabetic $directory/PropList.txt <$directory/UnicodeData.txt >$directory/$type.scm
scheme compile-unicode.scm fold $directory/UnicodeData.txt <$directory/CaseFolding.txt >$directory/fold.scm

for type in alphabetic downcase fold lone-lower lone-upper numeric space upcase; do
  cat >$directory/main.scm <<EOF
(import (scheme base) (scheme char) (scheme cxr) (scheme write) (stak char))

(write
  (case '$type
    ((space)
      (cddddr (cddddr $type-table)))
    (else
      $type-table)))
EOF

  stak $directory/main.scm >$directory/$type-out.scm

  diff $directory/$type-out.scm $directory/$type.scm
done
