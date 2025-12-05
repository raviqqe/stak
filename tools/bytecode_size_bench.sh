#!/bin/sh

set -e

. $(dirname $0)/utility.sh

cd $(dirname $0)/..

cargo build --release

export PATH=$PWD/target/release:$PATH

mkdir -p tmp

for file in $(list_scheme_files); do
  bytecode_file=tmp/bytecode_size/${file%.scm}.bc

  mkdir -p $(dirname $bytecode_file)
  cat prelude.scm $file | stak-compile --shake-tree >$bytecode_file
  echo $file $(wc -c <$bytecode_file)
done | tee tmp/bytecode_sizes.txt
