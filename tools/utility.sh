log() {
  echo '>>>' "$@" >&2
  "$@"
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
