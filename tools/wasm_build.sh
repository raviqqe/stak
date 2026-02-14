#!/bin/sh

set -e

cargo install wasm-pack

cd wasm

mkdir -p tmp

wasm-pack build --target web -- --features async
jq '. + input' pkg/package.json package.template.json >tmp/package.json
mv tmp/package.json pkg

wasm-pack pack
