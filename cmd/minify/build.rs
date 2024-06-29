use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    stak_build::build_r7rs()
}
