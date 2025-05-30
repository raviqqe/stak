log() (
  echo '>>>' "$@" >&2
  "$@"
)

list_scheme_files() (
  [ $# -eq 0 ]

  for file in $(git ls-files '*.scm' | grep -v prelude); do
    if [ -L $file ]; then
      continue
    fi

    echo $file
  done
)

build_binary() (
  cd $1
  shift 1

  cargo build --release
  cargo build --release "$@"
)

setup_bench() (
  [ $# -le 1 ]

  feature=$1

  brew install chibi-scheme gambit-scheme gauche guile lua micropython mruby ruby
  cargo install hyperfine

  case $feature in
  i63)
    build_options='--no-default-features --features std'
    ;;
  f62)
    build_options='--no-default-features --features std,float62'
    ;;
  esac

  build_binary . -p stak -p stak-interpret $build_options
  build_binary cmd/minimal -p mstak -p mstak-interpret

  export PATH=$PWD/target/release:$PWD/cmd/minimal/target/release:$PATH

  for file in bench/src/*/main.scm; do
    cat prelude.scm $file | stak-compile >${file%.scm}.bc
  done
)
