//! A build script.

#[cfg(any(feature = "cli", test))]
use stak_build::build_r7rs;
use stak_build::BuildError;

fn main() -> Result<(), BuildError> {
    #[cfg(any(feature = "cli", test))]
    build_r7rs()?;

    Ok(())
}
