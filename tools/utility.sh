setup_bench() {
  brew install chibi-scheme gauche guile
  cargo install hyperfine

  for directory in . cmd/minimal; do
    (
      cd $directory
      cargo build --release "$@"
    )
  done

  export PATH=$PWD/target/release:$PWD/cmd/minimal/target/release:$PATH

  for file in bench/src/*/main.scm; do
    cat prelude.scm $file | stak-compile >${file%.scm}.bc
  done
}
