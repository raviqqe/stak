//! A build sript.

use core::error::Error;
use stak_build::build_r7rs;

fn main() -> Result<(), Box<dyn Error>> {
    Ok(build_r7rs()?)
}
