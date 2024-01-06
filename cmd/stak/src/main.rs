//! A Stak Schemee interpreter.
//! machine.
//!
//! # Usage
//!
//! ```sh
//! stak foo.scm
//! ```

use stak_device::{ReadWriteDevice, StdioDevice};
use stak_macro::include_r7rs;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::Vm;
use std::{
    env,
    error::Error,
    fs::{read_to_string, File},
    io::{empty, Read},
};

const DEFAULT_HEAP_SIZE: usize = 1 << 21;
const COMPILER_PROGRAM: &[u8] = include_r7rs!("compile.scm");

fn main() -> Result<(), Box<dyn Error>> {
    let size = env::var("STAK_HEAP_SIZE")
        .ok()
        .map(|string| string.parse())
        .transpose()?
        .unwrap_or(DEFAULT_HEAP_SIZE);

    let mut source = Default::default();
    let mut program = vec![];

    read_source(source)?;
    compile(&source, &mut program, size)?;

    let mut heap = vec![Default::default(); size];
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?;

    vm.initialize(COMPILER_PROGRAM.iter().copied())?;

    Ok(vm.run()?)
}

fn read_source(source: &mut String) -> Result<(), io::Error> {
    for argument in env::args().skip(1) {
        File::open(argument)?.read_to_string(source)?;
    }

    Ok(())
}

fn compile(source: &str, target: &mut Vec<u8>, heap_size: usize) -> Result<(), Box<dyn Error>> {
    let mut heap = vec![Default::default(); heap_size];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(ReadWriteDevice::new(source.as_bytes(), target, empty())),
    )?;

    vm.initialize(COMPILER_PROGRAM.iter().copied())?;
    Ok(vm.run()?)
}
