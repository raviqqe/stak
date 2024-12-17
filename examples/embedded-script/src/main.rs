//! A `stak-build` example.

use stak::{
    build::include_bytecode,
    device::StdioDevice,
    file::VoidFileSystem,
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const HEAP_SIZE: usize = 1 << 16;
const FOO_BYTECODES: &[u8] = include_bytecode!("foo.scm");
const BAR_BYTECODES: &[u8] = include_bytecode!("bar.scm");

fn main() -> Result<(), Box<dyn Error>> {
    run(FOO_BYTECODES)?;
    run(BAR_BYTECODES)?;

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
