use crate::options::Options;
use main_error::MainError;
use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::StdioDevice;
use stak_file::OsFileSystem;
use stak_process_context::OsProcessContext;
use stak_r7rs::{SmallError, SmallPrimitiveSet};
use stak_time::OsClock;
use stak_vm::Vm;
use std::{
    env::args,
    fs::read,
    io::{Write, stdout},
    process::ExitCode,
};

pub fn main() -> Result<ExitCode, MainError> {
    let options = Options::parse(args());

    if options.version() {
        writeln!(
            stdout(),
            "{} {}",
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION")
        )?;

        return Ok(ExitCode::SUCCESS);
    }

    let file = options.file().ok_or("bytecode file missing")?;

    match Vm::new(
        vec![Default::default(); options.heap_size().unwrap_or(DEFAULT_HEAP_SIZE)],
        SmallPrimitiveSet::new(
            StdioDevice::new(),
            OsFileSystem::new(),
            OsProcessContext::new().with_argument_skip(0..options.last_index()),
            OsClock::new(),
        ),
    )?
    .run(read(file)?)
    {
        Ok(()) => Ok(ExitCode::SUCCESS),
        Err(SmallError::Halt(code)) => Ok(code.into()),
        Err(error) => Err(error.into()),
    }
}
