log() {
  echo '>>>' "$@" >&2
  "$@"
}

build_bench_binary() {
  (
    cd $1
    shift 1
    cargo build --release
    cargo build --release "$@"
  )
}

setup_bench() {
  [ $# -le 1 ]

  features=$1

  brew install chibi-scheme gauche guile
  cargo install hyperfine

  if echo $features | grep i >/dev/null; then
    stak_options='--no-default-features --features std'
  fi

  build_bench_binary . -p stak -p stak-interpret $stak_options
  build_bench_binary cmd/minimal -p mstak -p mstak-interpret

  export PATH=$PWD/target/release:$PWD/cmd/minimal/target/release:$PATH

  for file in bench/src/*/main.scm; do
    cat prelude.scm $file | stak-compile >${file%.scm}.bc
  done
}

list_scheme_files() {
  [ $# -eq 0 ]

  for file in $(git ls-files '*.scm' | grep -v prelude); do
    if [ -L $file ]; then
      continue
    fi

    echo $file
  done
}
