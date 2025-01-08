#!/bin/sh

set -ex

[ $# -eq 0 ]

cd $(dirname $0)/../..

. tools/utility.sh

setup_bench

cd bench/src

for directory in *; do
  base=$directory/main
  file=$base.scm

  scripts="$scripts${scripts:+,}stak $file,mstak $file,stak-interpret $base.bc,mstak-interpret $base.bc"
done

branch=${GITHUB_HEAD_REF:-$(basename $GITHUB_REF)}

if [ $branch = main ]; then
  options="--threshold-measure latency --threshold-test t_test --threshold-upper-boundary 0.99 --thresholds-reset"
else
  options="--start-point $GITHUB_BASE_REF --start-point-clone-thresholds --start-point-reset"
fi

bencher run \
  --adapter shell_hyperfine \
  --branch $branch \
  --err \
  --file results.json \
  --github-actions $GITHUB_TOKEN \
  --project stak \
  --testbed $RUNNER_ARCH$RUNNER_OS \
  --token $BENCHER_TOKEN \
  $options \
  hyperfine --export-json results.json -w 2 -N --input ../../compile.scm -L script "$scripts" "{script}"
