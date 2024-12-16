//! A `stak-build` example.

use stak_device::StdioDevice;
use stak_file::VoidFileSystem;
use stak_macro::include_bytecode;
use stak_process_context::VoidProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::Vm;
use core::error::Error;

const HEAP_SIZE: usize = 1 << 16;
const FOO_BYTECODES: &[u8] = include_bytecode!("foo.scm");
const BAR_BYTECODES: &[u8] = include_bytecode!("bar.scm");

fn main() -> Result<(), Box<dyn Error>> {
    run(FOO_BYTECODES)?;
    run(BAR_BYTECODES)?;

    Ok(())
}

fn run(bytecodes: &[u8]) -> Result<(), stak_r7rs::SmallError> {
    let mut heap = vec![Default::default(); HEAP_SIZE];
    let mut vm = Vm::new(
        &mut heap,
        SmallPrimitiveSet::new(
            StdioDevice::new(),
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    vm.initialize(bytecodes.iter().copied())?;
    vm.run()
}
