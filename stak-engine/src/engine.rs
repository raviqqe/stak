use stak_file::VoidFileSystem;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::Vm;

/// A scripting engine.
pub struct Engine {
    vm: Vm<SmallPrimitiveSet<Device, VoidFileSystem, VoidClock>>,
}

impl Engine {}
