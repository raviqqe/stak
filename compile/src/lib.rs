//! Stak Scheme bytecode compiler.

mod error;

use self::error::Error;
use device::ReadWriteDevice;
use primitive::SmallPrimitiveSet;
use std::io::empty;
use vm::Vm;

const DEFAULT_HEAP_SIZE: usize = 1 << 20;
const PRELUDE_SOURCE: &str = include_str!("prelude.scm");
const COMPILER_BYTECODES: &[u8] = include_bytes!(std::env!("STAK_BYTECODE_FILE"));

/// Compiles a program in R7RS Scheme into bytecodes.
///
/// # Examples
///
/// ```rust
/// let source = "(define x 42)";
/// let mut target = vec![];
///
/// stak_compile::compile_r7rs(source, &mut target);
/// ```
pub fn compile_r7rs(source: &str, target: &mut Vec<u8>) -> Result<(), Error> {
    compile_bare(&(PRELUDE_SOURCE.to_owned() + source), target)
}

/// Compiles a program in Scheme into bytecodes with only built-ins.
///
/// # Examples
///
/// ```rust
/// let source = "($$define x 42)";
/// let mut target = vec![];
///
/// stak_compile::compile_bare(source, &mut target);
/// ```
pub fn compile_bare(source: &str, target: &mut Vec<u8>) -> Result<(), Error> {
    let mut heap = vec![Default::default(); DEFAULT_HEAP_SIZE];
    let device = ReadWriteDevice::new(source.as_bytes(), target, empty());
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(device))?;

    vm.initialize(COMPILER_BYTECODES.iter().copied())?;
    vm.run()?;

    Ok(())
}
