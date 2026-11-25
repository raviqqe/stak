//! A build script.

use std::env::var;

fn main() {
    if cfg!(not(feature = "std")) && var("CARGO_CFG_TARGET_ENV").ok().as_deref() == Some("musl") {
        println!("cargo::rustc-link-arg=-lc");
    }
}
