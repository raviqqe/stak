//! A command to profile a bytecode file.
//!
//! # Usage
//!
//! ```sh
//! stak-profile --profile profile.txt foo.bc
//! ```

use clap::Parser;
use main_error::MainError;
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::StdioDevice;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::Vm;
use std::{
    fs::{read, OpenOptions},
    io::Write,
    path::PathBuf,
};

#[derive(clap::Parser)]
#[command(about, version)]
struct Arguments {
    #[arg(required(true))]
    bytecode_file: PathBuf,
    #[arg(short = 'p', long = "profile", required(true))]
    profile_file: PathBuf,
    #[arg(short = 's', long, default_value_t = DEFAULT_HEAP_SIZE)]
    heap_size: usize,
}

fn main() -> Result<(), MainError> {
    let arguments = Arguments::parse();
    let mut profile_file = OpenOptions::new()
        .write(true)
        .create(true)
        .open(&arguments.profile_file)?;

    let mut profiler = |id: &str, time| {
        writeln!(profile_file, "{id} {time}").unwrap();
    };

    let mut heap = vec![Default::default(); arguments.heap_size];
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?
        .with_profiler(&mut profiler);

    vm.initialize(read(&arguments.bytecode_file)?)?;

    Ok(vm.run()?)
}
