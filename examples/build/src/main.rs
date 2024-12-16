//! A `stak-build` example.

use stak_macro::include_bytecode;

static BYTECODES: &[u8] = include_bytecode!("src/main.scm");

fn main() {
    println!("Hello, world!");
}
