//! A command to interpret a bytecode file.
//!
//! # Usage
//!
//! ```sh
//! stak-interpret foo.bc
//! ```

mod options;

use self::options::Options;
use main_error::MainError;
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::StdioDevice;
use stak_file::OsFileSystem;
use stak_process_context::OsProcessContext;
use stak_r7rs::{SmallError, SmallPrimitiveSet};
use stak_time::OsClock;
use stak_vm::Vm;
use std::{
    env::args_os,
    fs::read,
    io::{Write, stdout},
    process::ExitCode,
};

fn main() -> Result<ExitCode, MainError> {
    let arguments = args_os().collect::<Vec<_>>();
    let options = Options::parse(
        arguments
            .iter()
            .map(|argument| argument.to_str().unwrap_or_default()),
    );

    if options.version() {
        writeln!(
            stdout(),
            "{} {}",
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION")
        )?;

        return Ok(ExitCode::SUCCESS);
    }

    let index = options.file_index().ok_or("bytecode file missing")?;

    match Vm::new(
        vec![Default::default(); options.heap_size().unwrap_or(DEFAULT_HEAP_SIZE)],
        SmallPrimitiveSet::new(
            StdioDevice::new(),
            OsFileSystem::new(),
            OsProcessContext::new().with_argument_skip(index),
            OsClock::new(),
        ),
    )?
    .run(read(&arguments[index])?)
    {
        Ok(()) => Ok(ExitCode::SUCCESS),
        Err(SmallError::Halt(code)) => Ok(code.into()),
        Err(error) => Err(error.into()),
    }
}
