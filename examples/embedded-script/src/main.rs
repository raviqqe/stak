//! A `stak-build` example.

use core::error::Error;
use stak::{
    device::StdioDevice,
    file::VoidFileSystem,
    include_bytecode,
    process_context::VoidProcessContext,
    program::Program,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const HEAP_SIZE: usize = 1 << 16;
const FOO_PROGRAM: Program = include_bytecode!("foo.scm");
const BAR_PROGRAM: Program = include_bytecode!("bar.scm");

fn main() -> Result<(), Box<dyn Error>> {
    run(&FOO_PROGRAM.bytecode())?;
    run(&BAR_PROGRAM.bytecode())?;

    Ok(())
}

fn run(bytecodes: &[u8]) -> Result<(), SmallError> {
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
