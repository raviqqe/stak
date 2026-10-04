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
use stak_file::OsFileSystem;
use stak_process_context::OsProcessContext;
use stak_r7rs::{SmallError, SmallPrimitiveSet};
use stak_time::OsClock;
use stak_vm::Vm;
use std::{fs::read, path::PathBuf, process::ExitCode};

#[derive(clap::Parser)]
#[command(about, version)]
struct Arguments {
    #[arg(required(true))]
    file: PathBuf,
    #[arg()]
    arguments: Vec<String>,
    #[arg(short = 's', long, default_value_t = DEFAULT_HEAP_SIZE)]
    heap_size: usize,
}

fn main() -> Result<ExitCode, MainError> {
    let arguments = Arguments::parse();

    match Vm::new(
        vec![Default::default(); arguments.heap_size],
        SmallPrimitiveSet::new(
            StdioDevice::new(),
            OsFileSystem::new(),
            OsProcessContext::new().with_argument_skip(1),
            OsClock::new(),
        ),
    )?
    .run(read(&arguments.file)?)
    {
        Ok(()) => Ok(ExitCode::SUCCESS),
        Err(SmallError::Halt(code)) => Ok(code.into()),
        Err(error) => Err(error.into()),
    }
}
