#!/bin/sh

set -ex

cd $(dirname $0)/..

mkdir -p tmp

cat <<EOF >tmp/main.scm
(import (scheme base) (scheme write))

(define (foo x)
  (display x))
EOF

cat prelude.scm tmp/main.scm | stak compile.scm >tmp/main.bc
