//! Scheme source code minifier.

use stak_configuration::DEFAULT_HEAP_SIZE;
use stak_device::reaReadWriteDevice;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::Vm;
use std::error::Error;
use std::io::{empty, Read, Write};

/// Minifies given source codes.
pub fn minify(reader: impl Read, writer: impl Write) -> Result<(), Box<dyn Error>> {
    const PROGRAM: &[u8] = stak_macro::include_r7rs!("minify.scm");

    let mut heap = [Default::default(); DEFAULT_HEAP_SIZE];
    let device = ReadWriteDevice::new(reader, writer, empty());
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(device)).unwrap();

    vm.initialize(PROGRAM.iter().copied())?;
    vm.run()?;

    Ok(())
}
