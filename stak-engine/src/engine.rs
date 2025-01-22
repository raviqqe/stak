use stak_device::StdioDevice;
use stak_file::VoidFileSystem;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::Vm;

/// A scripting engine.
pub struct Engine {
    vm: Vm<SmallPrimitiveSet<StdioDevice, VoidFileSystem, VoidProcessContext, VoidClock>>,
}

impl Engine {
    pub fn new() -> Self {
        Vm::new(SmallPrimitiveSet::new(
            Default::default(),
            Default::default(),
            Default::default(),
            Default::default(),
        ))
        .map(|vm| Engine { vm })
        .unwrap()
    }
}
