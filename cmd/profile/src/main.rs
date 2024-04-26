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
use stak_profiler::{calculate_durations, parse_records, StackProfiler};
use stak_vm::Vm;
use std::{
    fs::{read, OpenOptions},
    io::{stdin, stdout},
    path::PathBuf,
};

#[derive(clap::Parser)]
#[command(about, version)]
struct Arguments {
    #[command(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand)]
enum Command {
    /// Runs a bytecode file.
    Run(RunArguments),
    /// Analyze profile records.
    Analyze(AnalyzeArguments),
}

#[derive(clap::Args)]
struct RunArguments {
    /// A bytecode file.
    #[arg(required(true))]
    bytecode_file: PathBuf,
    /// A profile file to which a profiler writes records.
    #[arg(short = 'p', long = "profile", required(true))]
    profile_file: PathBuf,
    /// A heap size of a virtual machine.
    #[arg(short = 's', long, default_value_t = DEFAULT_HEAP_SIZE)]
    heap_size: usize,
}

#[derive(clap::Args)]
struct AnalyzeArguments {
    #[command(subcommand)]
    command: Analysis,
}

#[derive(clap::Subcommand)]
enum Analysis {
    /// Calculates procedure durations.
    Duration,
}

fn main() -> Result<(), MainError> {
    match Arguments::parse().command {
        Command::Run(arguments) => {
            let mut profiler = StackProfiler::new(
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
        Command::Analyze(arguments) => match arguments.command {
            Analysis::Duration => Ok(calculate_durations(
                parse_records(stdin().lock()),
                stdout(),
            )?),
        },
    }
}
