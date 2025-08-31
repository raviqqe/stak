#!/bin/sh

set -e

cargo install wasm-pack

cd wasm

cargo build
wasm-pack build --release --target web -- --features async
jq '. + input' pkg/package.json package.json >/tmp/package.json
mv /tmp/package.json pkg

wasm-pack pack wasm
