#!/bin/sh

set -ex

build_feature() {
  cargo build --release --features $1
  cp target/release/stak target/release/stak-$1
}

brew install chicken gambit-scheme
cargo install hyperfine

build_feature boost

cargo build --release

(
  cd $(dirname $0)/..

  for file in $(find bench -type f -name '*.scm' | sort); do
    base=${file%.scm}

    cat prelude.scm $file | tools/compile.sh >$base.out

    hyperfine \
      --sort command \
      "target/release/stak $base.out" \
      "target/release/stak-boost $base.out" \
      "gsi $file" \
      "csi -s $file" \
      "python3 $base.py"
  done
)
