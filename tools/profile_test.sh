#!/bin/sh

set -ex

cd $(dirname $0)/..

mkdir -p tmp

cat <<EOF >tmp/main.scm
(import (scheme base) (scheme write))

(define (foo x)
  (display x))
EOF

stak compile.scm <prelude.scm <tmp/main.scm >tmp/main.bc
