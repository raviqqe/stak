#!/bin/sh

set -e

cargo clippy "$@" -- -D warn -D clippy::use_self
