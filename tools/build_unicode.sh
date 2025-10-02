#!/bin/sh

set -ex

cd $(dirname $0)/..

mkdir -p tmp

curl -fsSL https://raw.githubusercontent.com/unicode-org/icu/24ebdccde39af10d8564fb7d50ad7f11a26e86b9/icu4c/source/data/unidata/CaseFolding.txt >tmp/folding.txt
