//! A command to interpret a bytecode file.
//!
//! # Usage
//!
//! ```sh
//! stak-interpret foo.bc
//! ```

use clap::Parser;
use main_error::MainError;
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::StdioDevice;
use stak_file::LibcFileSystem;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::Vm;
use std::{fs::read, path::PathBuf};

#[derive(clap::Parser)]
#[command(about, version)]
struct Arguments {
    #[arg(required(true))]
    file: PathBuf,
    #[arg(short = 's', long, default_value_t = DEFAULT_HEAP_SIZE)]
    heap_size: usize,
}

fn main() -> Result<(), MainError> {
    let arguments = Arguments::parse();

    let mut heap = vec![Default::default(); arguments.heap_size];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(StdioDevice::new(), LibcFileSystem::new()),
    )?;

    vm.initialize(read(&arguments.file)?)?;

    Ok(vm.run()?)
}
