//! A command to profile a bytecode file.
//!
//! # Usage
//!
//! ```sh
//! stak-profile --profile profile.txt foo.bc
//! ```

mod profiler;

use clap::Parser;
use main_error::MainError;
use profiler::WriteProfiler;
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::StdioDevice;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::Vm;
use std::{
    fs::{read, OpenOptions},
    path::PathBuf,
};

#[derive(clap::Parser)]
#[command(about, version)]
struct Arguments {
    #[command(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand)]
#[command()]
enum Command {
    #[command()]
    Run(RunArguments),
}

#[derive(clap::Args)]
#[command()]
struct RunArguments {
    #[arg(required(true))]
    bytecode_file: PathBuf,
    #[arg(short = 'p', long = "profile", required(true))]
    profile_file: PathBuf,
    #[arg(short = 's', long, default_value_t = DEFAULT_HEAP_SIZE)]
    heap_size: usize,
}

fn main() -> Result<(), MainError> {
    match Arguments::parse().command {
        Command::Run(arguments) => {
            let mut profiler = WriteProfiler::new(
                OpenOptions::new()
                    .write(true)
                    .create(true)
                    .truncate(true)
                    .open(&arguments.profile_file)?,
            );
            let mut heap = vec![Default::default(); arguments.heap_size];
            let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?
                .with_profiler(&mut profiler);

            vm.initialize(read(&arguments.bytecode_file)?)?;

            Ok(vm.run()?)
        }
    }
}
