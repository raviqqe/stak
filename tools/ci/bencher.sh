#!/bin/sh

set -e

while getopts o: option; do
  case $option in
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
    --branch "$GITHUB_HEAD_REF" \
    --err \
    --file results.json \
    --github-actions $GITHUB_TOKEN \
    --project stak \
    --start-point "$GITHUB_BASE_REF" \
    --start-point-clone-thresholds \
    --start-point-reset \
    --testbed $os \
    --threshold-measure latency \
    --threshold-test t_test \
    --threshold-upper-boundary 0.99 \
    --thresholds-reset \
    --token $BENCHER_TOKEN \
    $hyperfine --export-json results.json "$@"
}

. tools/utility.sh
bench
