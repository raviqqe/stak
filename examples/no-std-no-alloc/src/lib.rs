//! An example library to run the Stak Scheme virtual machines without `std` and
//! `alloc` crates.

#![no_std]

use core::{
    fmt::{self, Write},
    num::ParseIntError,
    str::{self, FromStr, Utf8Error},
};
use heapless::String;
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

/// Calculates the Fibonacci number.
pub fn fibonacci(number: usize) -> Result<usize, FibonacciError> {
    let mut input = String::<8>::new();

    write!(&mut input, "{number}")?;

    // Initialize an in-memory I/O device.
    let mut device = FixedBufferDevice::<BUFFER_SIZE, BUFFER_SIZE>::new(input.as_bytes());

    run_vm(&include_module!("fibonacci.scm").bytecode(), &mut device)?;

    // If stderr is not empty, we assume that some error has occurred.
    if !device.error().is_empty() {
        return Err(FibonacciError::Unknown);
    }

    // Decode the output.
    Ok(usize::from_str(str::from_utf8(device.output())?)?)
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

/// A fibonacci calculation error.
#[derive(Debug)]
pub enum FibonacciError {
    /// A value format error.
    Format(fmt::Error),
    /// Integer parse failure.
    ParseInt(ParseIntError),
    /// A failure in R7RS-small primitives.
    Small(SmallError),
    /// An unknown error.
    Unknown,
    /// UTF-8 parse failure.
    Utf8(Utf8Error),
}

impl From<fmt::Error> for FibonacciError {
    fn from(error: fmt::Error) -> Self {
        Self::Format(error)
    }
}

impl From<ParseIntError> for FibonacciError {
    fn from(error: ParseIntError) -> Self {
        Self::ParseInt(error)
    }
}

impl From<SmallError> for FibonacciError {
    fn from(error: SmallError) -> Self {
        Self::Small(error)
    }
}

impl From<Utf8Error> for FibonacciError {
    fn from(error: Utf8Error) -> Self {
        Self::Utf8(error)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn calculate() {
        assert_eq!(fibonacci(0).unwrap(), 0);
        assert_eq!(fibonacci(1).unwrap(), 1);
        assert_eq!(fibonacci(2).unwrap(), 1);
        assert_eq!(fibonacci(3).unwrap(), 2);
        assert_eq!(fibonacci(4).unwrap(), 3);
        assert_eq!(fibonacci(5).unwrap(), 5);
        assert_eq!(fibonacci(6).unwrap(), 8);
    }
}
