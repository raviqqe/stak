//! A build script.

use stak_build::{build_r7rs, BuildError};

fn main() -> Result<(), BuildError> {
    build_r7rs()
}
