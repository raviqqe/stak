setup_bench() {
  brew install chibi-scheme gambit-scheme gauche
  cargo install hyperfine

  for directory in . cmd/minimal; do
    (
      cd $directory
      cargo build --release "$@"
    )
  done

  export PATH=$PWD/target/release:$PWD/cmd/minimal/target/release:$PATH

  for directory in bench/src/*; do
    base=$directory/main

    cat prelude.scm $base.scm | stak-compile >$base.bc
  done
}
