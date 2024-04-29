#!/bin/sh

set -ex

cd $(dirname $0)/..

export PATH=$PWD/target/release:$PATH
cargo build --release

mkdir -p tmp
cat <<EOF >tmp/main.scm
(import (scheme base) (scheme write))

(define (foo x)
  (display x))

(foo 42)
EOF

cat prelude.scm compile.scm | stak compile.scm >tmp/main.bc
stak-profile run --profile tmp/profile.txt tmp/main.bc <prelude.scm <compile.scm
stak-profile analyze duration <tmp/profile.txt |
  stak-profile analyze stack-collapse |
  stak-profile analyze stack-reverse |
  stak-profile analyze flamegraph >tmp/flamegraph.txt
