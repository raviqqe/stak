#!/bin/sh

set -ex

version=17.0.0

fetch_data() (
  curl -fsSL https://raw.githubusercontent.com/unicode-org/unicodetools/main/unicodetools/data/ucd/$version/$1.txt
)

compile() (
  type=$1
  input=$2

  shift 2

  for name in $@; do
    arguments="$arguments $directory/$name.txt"
  done

  ${STAK_HOST:-stak} compile-unicode.scm $type $arguments <$directory/$input.txt >$directory/$type.scm
)

cd $(dirname $0)/..

directory=tmp/unicode

mkdir -p $directory

cargo build --profile release_test

export PATH=$PWD/target/release_test:$PATH

for base in CaseFolding PropList UnicodeData; do
  fetch_data $base >$directory/$base.txt
done

for type in alphabetic downcase lone-lower lone-upper numeric space upcase; do
  compile $type UnicodeData
done

compile fold CaseFolding UnicodeData
compile other-alphabetic PropList

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
