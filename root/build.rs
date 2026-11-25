//! A build script.

use stak_build::{BuildError, build_r7rs};
use std::env::var;

fn main() -> Result<(), BuildError> {
    if cfg!(not(feature = "std")) && var("CARGO_CFG_TARGET_ENV").ok().as_deref() == Some("musl") {
        println!("cargo::rustc-link-arg=-lc");
    }

    build_r7rs()
}
