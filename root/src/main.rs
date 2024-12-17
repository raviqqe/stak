//! A Stak Scheme interpreter.
//!
//! # Usage
//!
//! ```sh
//! stak foo.scm
//! ```

use clap::{self, Parser};
use main_error::MainError;
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::StdioDevice;
use stak_file::OsFileSystem;
use stak_macro::include_bytecode;
use stak_process_context::OsProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::OsClock;
use stak_vm::Vm;

#[derive(clap::Parser)]
#[command(about, version)]
struct Arguments {
    #[arg()]
    arguments: Vec<String>,
    #[arg(short = 's', long, default_value_t = DEFAULT_HEAP_SIZE)]
    heap_size: usize,
}

fn main() -> Result<(), MainError> {
    let arguments = Arguments::parse();

    let mut heap = vec![Default::default(); arguments.heap_size];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            StdioDevice::new(),
            OsFileSystem::new(),
            OsProcessContext::new(),
            OsClock::new(),
        ),
    )?;

    vm.initialize(include_bytecode!("main.scm").iter().copied())?;

    Ok(vm.run()?)
}
