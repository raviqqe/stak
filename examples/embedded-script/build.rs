//! A build script.

use core::error::Error;
use stak::build::build_r7rs;

fn main() -> Result<(), Box<dyn Error>> {
    Ok(build_r7rs()?)
}
