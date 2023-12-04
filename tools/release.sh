#!/bin/sh

set -e

cargo release --exclude stak-compile "$@"
