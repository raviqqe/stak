log() (
  echo '>>>' "$@" >&2
  "$@"
)

list_scheme_files() (
  [ $# -eq 0 ]

  for file in $(git ls-files '*.scm' | grep -v -e prelude -e tools); do
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

log_version() (
  if which $1 >/dev/null; then
    log "$@"
  fi
)

log_versions() (
  log_version chibi-scheme -V
  log_version gsi -v
  log_version gosh -V
  log_version lua -v

  for command in guile micropython mruby python3 ruby stak; do
    log_version $command --version
  done
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

  export PATH=$PWD/target/release:$PATH

  for file in bench/src/*/main.scm; do
    cat prelude.scm $file | stak-compile >${file%.scm}.bc
  done

  set +x
  log_versions
)

format_metrics() (
  input_file=$1
  output_file=$2

  cat >$output_file <<EOF
{
  "key": "bytecode_sizes",
  "name":"Bytecode sizes",'
  "metrics": [
$(
    first=true

    while read -r line; do
      if $first; then
        first=false
      fi

      key=$(echo $line | cut -d ' ' -f 1)
      value=$(echo $line | cut -d ' ' -f 2)

      cat <<EOF
{
  "key": "$line"
}
EOF
    done <$input_file
  )
  ],
  "acceptables": [
  ]
}
EOF
)
