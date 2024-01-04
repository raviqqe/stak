//! A command to interpret a bytecode file.
//!
//! # Usage
//!
//! ```sh
//! stak-interpret < foo.bc
//! ```

use device::StdioDevice;
use primitive::SmallPrimitiveSet;
use std::{
    env::{self, args},
    error::Error,
    fs::read,
    process::exit,
};
use vm::Vm;

const DEFAULT_HEAP_SIZE: usize = 1 << 21;

fn main() {
    if let Err(error) = run() {
        eprintln!("{}", error);
        exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let size = env::var("STAK_HEAP_SIZE")
        .ok()
        .map(|string| string.parse())
        .transpose()?
        .unwrap_or(DEFAULT_HEAP_SIZE);
    let mut heap = vec![Default::default(); size];
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?;

    vm.initialize(read(args().nth(1).ok_or(format!(
        "Usage: {} <bytecode_file>",
        args().next().expect("command name")
    ))?)?)?;

    Ok(vm.run()?)
}
