#!/bin/sh

set -e

cargo install wasm-pack

cd wasm

wasm-pack build --target web -- --features async
jq '. + input' pkg/package.json package.json >/tmp/package.json
mv /tmp/package.json pkg

wasm-pack pack
