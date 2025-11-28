#!/bin/sh

set -ex

update_bytecode() (
  bytecode_file=compiler/src/compile.bc

  cat prelude.scm compile.scm | stak compile.scm >$bytecode_file

  git add -f $bytecode_file
)

update_cargo_toml() (
  for directory in $(git ls-files '*/src/main.rs' | sed s%/src/main.rs%%) wasm; do
    cargo_file=$directory/Cargo.toml

    for profile in dev release; do
      cat <<EOF
[profile.$profile]
codegen-units = 1
lto = true
opt-level = 3
panic = "abort"
strip = true

[profile.$profile.build-override]
opt-level = 3
debug-assertions = false
overflow-checks = false
EOF
    done >>$cargo_file

    git add $cargo_file
  done
)

update_bytecode
update_cargo_toml

git config user.email action@github.com
git config user.name 'GitHub Action'
git commit -m release

cargo install cargo-workspaces

for directory in . cmd/minimal; do
  (
    cd $directory
    cargo workspaces publish -y --from-git "$@"
  )
done
