#!/bin/sh

set -e

while getopts b:o: option; do
  case $option in
  b)
    branch=$OPTARG
    ;;
  o)
    os=$OPTARG
    ;;
  esac
done

shift $(expr $OPTIND - 1)

[ -n "$os" ]
[ $# -eq 0 ]

cd $(dirname $0)/../..

cargo install hyperfine
hyperfine=$(which hyperfine)

hyperfine() {
  bencher run \
    --adapter shell_hyperfine \
    --branch $branch \
    --err \
    --file results.json \
    --github-actions $GITHUB_TOKEN \
    --project stak \
    --testbed $os \
    --threshold-max-sample-size 10 \
    --threshold-measure latency \
    --threshold-test t_test \
    --threshold-upper-boundary 0.99 \
    --thresholds-reset \
    --token $BENCHER_TOKEN \
    $hyperfine --export-json results.json "$@"
}

. tools/utility.sh
bench
