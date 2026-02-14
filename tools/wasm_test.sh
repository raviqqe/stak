#!/bin/sh

set -e

cd wasm
wasm-pack test --node
