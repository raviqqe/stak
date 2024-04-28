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
EOF

cat prelude.scm tmp/main.scm | stak compile.scm >tmp/main.bc
stak-profile run --profile tmp/profile.txt tmp/main.bc
stak-profile analyze duration <tmp/profile.txt >tmp/durations.txt
stak-profile analyze stack-collapse <tmp/durations.txt >tmp/collapse.txt
stak-profile analyze flamegraph <tmp/collapse.txt >tmp/flamegraph.txt
