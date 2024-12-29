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
    --branch "${GITHUB_HEAD_REF:-$(basename $GITHUB_REF)}" \
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

brew install chibi-scheme gambit-scheme gauche
cargo install hyperfine

for directory in . cmd/minimal; do
  (
    cd $directory
    cargo build --release $build_options
  )
done

export PATH=$PWD/target/release:$PWD/cmd/minimal/target/release:$PATH

filter=.

if [ $# -gt 0 ]; then
  filter="$@"
fi

for file in $(find bench -type f -name '*.scm' | sort | grep $filter); do
  base=${file%.scm}

  cat prelude.scm $file | stak-compile >$base.bc

  scripts="stak $file,mstak $file,stak-interpret $base.bc,mstak-interpret $base.bc,gsi $file,chibi-scheme $file,gosh $file"

  if [ -r $base.py ]; then
    scripts="$scripts,python3 $base.py"
  fi

  hyperfine --sort command --input compile.scm -L script "$scripts" "{script}"
done
