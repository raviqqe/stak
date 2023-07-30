#!/bin/sh

set -ex

. $(dirname $0)/utility.sh

cargo test $(feature_flags)
