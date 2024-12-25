//! A build script.

use stak_build::{build_r7rs, BuildError};

fn main() -> Result<(), BuildError> {
    #[cfg(any(test, feature = "cli"))]
    build_r7rs()?;

    Ok(())
}
