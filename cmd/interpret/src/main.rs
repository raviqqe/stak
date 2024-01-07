//! A command to interpret a bytecode file.
//!
//! # Usage
//!
//! ```sh
//! stak-interpret foo.bc
//! ```

use clap::Parser;
use stak_device::StdioDevice;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::Vm;
use std::{error::Error, fs::read, process::exit};

#[derive(clap::Parser)]
#[command(about, version)]
struct Arguments {
    #[arg(required(true))]
    file: String,
    #[arg(short = 's', long, default_value_t = 1 << 20)]
    heap_size: usize,
}

fn main() {
    if let Err(error) = run() {
        eprintln!("{}", error);
        exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let arguments = Arguments::parse();

    let mut heap = vec![Default::default(); arguments.heap_size];
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?;

    vm.initialize(read(arguments.file)?)?;

    Ok(vm.run()?)
}
