#!/bin/sh

set -ex

build_feature() {
  cargo build --release --features $feature
  cp target/release/stak target/release/stak-$feature
}

brew install gambit-scheme
cargo install hyperfine

build_feature boost
build_feature gc_always

cargo build --release

(
  cd $(dirname $0)/..

  for file in $(find bench -type f -name '*.scm'); do
    base=${file%.scm}

    cat prelude.scm $file | tools/compile.sh >$base.out

    hyperfine \
      --sort command \
      "target/release/stak $base.out" \
      "target/release/stak-boost $base.out" \
      "target/release/stak-gc_always $base.out" \
      "gsi $file" \
      "python3 $base.py"
  done
)
