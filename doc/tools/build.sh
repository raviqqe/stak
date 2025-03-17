#!/bin/sh

set -e

mkdir -p public
curl -fsSL https://stak-lang.s3.amazonaws.com/icon.svg >public/icon.svg

astro build "$@"
