//! Stak Scheme bytecode compiler.

use device::ReadWriteDevice;
use primitive::SmallPrimitiveSet;
use std::{error::Error, io::empty};
use vm::Vm;

const HEAP_SIZE: usize = 1 << 20;
const PRELUDE_SOURCE: &str = include_str!("prelude.scm");
const COMPILER_BYTECODES: &[u8] = include_bytes!(std::env!("STAK_BYTECODE_FILE"));

/// Compiles a program in R7RS Scheme into bytecodes.
///
/// # Examples
///
/// ```rust
/// let bytecodes = stak_macro::compile_r7rs!("(define x 42)");
/// ```
pub fn compile_r7rs(source: &str, target: &mut Vec<u8>) -> Result<(), Box<dyn Error>> {
    compile_bare(&(PRELUDE_SOURCE.to_owned() + source), target)
}

/// Compiles a program in Scheme into bytecodes with only built-ins.
///
/// # Examples
///
/// ```rust
/// let bytecodes = stak_macro::compile_naked!("($$define x 42)");
/// ```
pub fn compile_bare(source: &str, target: &mut Vec<u8>) -> Result<(), Box<dyn Error>> {
    let mut heap = vec![Default::default(); HEAP_SIZE];
    let device = ReadWriteDevice::new(source.as_bytes(), vec![], empty());
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(device))?;

    vm.initialize(COMPILER_BYTECODES.iter().copied())?;

    vm.run()?;

    Ok(())
}
