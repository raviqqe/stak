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
use stak_profiler::{
    calculate_durations, calculate_flamegraph, collapse_stacks, write_records, DurationRecord,
    ProcedureRecord, StackProfiler, StackedRecord,
};
use stak_vm::Vm;
use std::{
    fs::{read, OpenOptions},
    io::{stdin, stdout, BufRead, BufWriter},
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
    /// Calculates collapsed stacks.
    StackCollapse,
    /// Calculates a flamegraph.
    Flamegraph,
}

fn main() -> Result<(), MainError> {
    match Arguments::parse().command {
        Command::Run(arguments) => {
            let mut profiler = StackProfiler::new(BufWriter::new(
                OpenOptions::new()
                    .write(true)
                    .create(true)
                    .truncate(true)
                    .open(&arguments.profile_file)?,
            ));
            let mut heap = vec![Default::default(); arguments.heap_size];
            let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?
                .with_profiler(&mut profiler);

            vm.initialize(read(&arguments.bytecode_file)?)?;
            vm.run()?;
        }
        Command::Analyze(arguments) => {
            let reader = stdin().lock();
            let writer = BufWriter::new(stdout().lock());

            match arguments.command {
                Analysis::Duration => write_records(
                    calculate_durations(reader.lines().map(|line| {
                        let mut record = line?.parse::<ProcedureRecord>()?;
                        record.stack_mut().reverse_frames();
                        Ok(record)
                    })),
                    writer,
                )?,
                Analysis::StackCollapse => write_records(
                    collapse_stacks(reader.lines().map(|line| line?.parse::<DurationRecord>())),
                    writer,
                )?,
                Analysis::Flamegraph => {
                    calculate_flamegraph(reader.lines().map(|line| line?.parse()), writer)?
                }
            }
        }
    }

    Ok(())
}
