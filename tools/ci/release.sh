#!/bin/sh

set -ex

bytecode_file=compiler/src/compile.bc

update_bytecode() {
  cat prelude.scm compile.scm | stak compile.scm >$bytecode_file

  git add -f $bytecode_file
}

update_cargo_toml() {
  for main_file in $(git ls-files '*/src/main.rs'); do
    cat <<EOF >>$(dirname $main_file)/../Cargo.toml
[profile.release.build-override]
codegen-units = 1
lto = true
panic = "abort"
strip = true
EOF
  done

  git add .
}

if [ $# -ne 0 ]; then
  exit 1
fi

cargo install cargo-workspaces

update_bytecode
update_cargo_toml

git config user.email action@github.com
git config user.name 'GitHub Action'
git commit -m release

for directory in . cmd/minimal; do
  (
    cd $directory
    cargo workspaces publish -y --from-git
  )
done
