//! A `stak-build` example.

use core::error::Error;
use stak::{
    device::StdioDevice,
    file::VoidFileSystem,
    include_module,
    module::{Module, UniversalModule},
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

const HEAP_SIZE: usize = 1 << 16;

static FOO_MODULE: UniversalModule = include_module!("foo.scm");
static BAR_MODULE: UniversalModule = include_module!("bar.scm");

fn main() -> Result<(), Box<dyn Error>> {
    run(&FOO_MODULE.bytecode())?;
    run(&BAR_MODULE.bytecode())?;

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
