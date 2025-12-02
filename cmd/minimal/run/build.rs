//! A build script.

use std::env::var;

fn main() {
    if var("CARGO_CFG_TARGET_ENV").ok().as_deref() == Some("musl") {
        println!("cargo::rustc-link-lib=c");
    }
}
