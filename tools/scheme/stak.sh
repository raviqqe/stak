#!/bin/sh

set -e

export PATH=$(dirname $0)/../../target/release_test

stak "$@"
