//! A Stak Schemee interpreter.
//! machine.
//!
//! # Usage
//!
//! ```sh
//! stak foo.scm
//! ```

use stak_device::StdioDevice;
use stak_macro::include_r7rs;
use stak_primitive::SmallPrimitiveSet;
use stak_vm::Vm;
use std::{env, error::Error};

const DEFAULT_HEAP_SIZE: usize = 1 << 21;
const COMPILER_PROGRAM: &[u8] = include_r7rs!("compile.scm");

fn main() -> Result<(), Box<dyn Error>> {
    let size = env::var("STAK_HEAP_SIZE")
        .ok()
        .map(|string| string.parse())
        .transpose()?
        .unwrap_or(DEFAULT_HEAP_SIZE);
    let mut heap = vec![Default::default(); size];
    let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(StdioDevice::new()))?;

    vm.initialize(COMPILER_PROGRAM.iter().copied())?;

    Ok(vm.run()?)
}
