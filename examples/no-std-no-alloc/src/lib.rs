//! An exampe library to run the Stak Scheme virtual machines without `std` and `alloc` crates.

#![no_std]

use core::{
    num::ParseIntError,
    str::{self, FromStr, Utf8Error},
};
use stak::{
    device::FixedBufferDevice,
    file::VoidFileSystem,
    include_module,
    module::{Module, UniversalModule},
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const BUFFER_SIZE: usize = 1 << 8;
const HEAP_SIZE: usize = 1 << 16;

static MODULE: UniversalModule = include_module!("fibonacci.scm");

/// Calculates the Fibonacci number.
pub fn fibonacci(input: &str) -> Result<isize, MyError> {
    // Initialize an in-memory I/O device.
    let mut device = FixedBufferDevice::<BUFFER_SIZE, BUFFER_SIZE>::new(input.as_bytes());

    run_vm(&MODULE.bytecode(), &mut device)?;

    // If stderr is not empty, we assume that some error has occurred.
    if !device.error().is_empty() {
        return Err(MyError::InvalidInput);
    }

    // Decode the output.
    Ok(isize::from_str(&str::from_utf8(device.output())?)?)
}

fn run_vm(
    bytecodes: &[u8],
    device: &mut FixedBufferDevice<BUFFER_SIZE, BUFFER_SIZE>,
) -> Result<(), SmallError> {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            // Attach an I/O device.
            device,
            // For the rest, you use "void" interfaces because they are not
            // needed.
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(bytecodes.iter().copied())?;
    vm.run()
}

/// My error.
pub enum MyError {
    /// Invalid input.
    InvalidInput,
    /// Integer parse failure.
    ParseInt(ParseIntError),
    /// A failure in R7RS-small primitives.
    Small(SmallError),
    /// UTF-8 parse failure.
    Utf8(Utf8Error),
}

impl From<ParseIntError> for MyError {
    fn from(error: ParseIntError) -> Self {
        Self::ParseInt(error)
    }
}

impl From<SmallError> for MyError {
    fn from(error: SmallError) -> Self {
        Self::Small(error)
    }
}

impl From<Utf8Error> for MyError {
    fn from(error: Utf8Error) -> Self {
        Self::Utf8(error)
    }
}
