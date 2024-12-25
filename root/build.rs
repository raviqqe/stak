//! A build script.

#[cfg(any(test, feature = "cli"))]
use stak_build::build_r7rs;
use stak_build::BuildError;

fn main() -> Result<(), BuildError> {
    #[cfg(any(test, feature = "cli"))]
    build_r7rs()?;

    Ok(())
}
