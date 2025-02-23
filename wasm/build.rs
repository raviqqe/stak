//! A build script.

use stak_build::{BuildError, build_r7rs};

fn main() -> Result<(), BuildError> {
    build_r7rs()
}
