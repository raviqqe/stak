//! Stak Scheme bytecode compiler.

use device::ReadWriteDevice;
use primitive::SmallPrimitiveSet;
use std::{error::Error, io::empty};
use vm::Vm;

const PRELUDE_SOURCE: &str = include_str!("prelude.scm");
const COMPILER_BYTECODES: &[u8] = include_bytes!(std::env!("STAK_BYTECODE_FILE"));

/// Compiles a program in R7RS Scheme into bytecodes.
///
/// # Examples
///
/// ```rust
/// let bytecodes = stak_macro::compile_r7rs!("(define x 42)");
/// ```
pub fn compile_r7rs(
    source: &str,
    target: &mut Vec<u8>,
    heap_size: usize,
) -> Result<(), Box<dyn Error>> {
    compile_bare(&(PRELUDE_SOURCE.to_owned() + source), target, heap_size)
}

/// Compiles a program in Scheme into bytecodes with only built-ins.
///
/// # Examples
///
/// ```rust
/// let bytecodes = stak_macro::compile_naked!("($$define x 42)");
/// ```
pub fn compile_bare(
    source: &str,
    target: &mut Vec<u8>,
    heap_size: usize,
) -> Result<(), Box<dyn Error>> {
    let mut heap = vec![Default::default(); heap_size];
    let device = ReadWriteDevice::new(source.as_bytes(), target, empty());
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(device))?;

    vm.initialize(COMPILER_BYTECODES.iter().copied())?;

    vm.run()?;

    Ok(())
}
