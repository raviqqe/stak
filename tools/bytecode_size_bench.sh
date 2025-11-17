#!/bin/sh

set -e

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

cargo build --release

export PATH=$PWD/target/release:$PATH

for file in $(list_scheme_files); do
  cat prelude.scm $file | stak-compile --shake-tree >main.bc
  echo $file $(wc -c <main.bc)
done
