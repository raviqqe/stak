use cfg_if::cfg_if;
use stak_file::VoidFileSystem;
use stak_module::Module;
use stak_process_context::VoidProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::{Error, Value, Vm};

cfg_if! {
    if #[cfg(feature = "std")] {
        type Device = stak_device::StdioDevice;
    } else {
        type Device = stak_device::VoidDevice;
    }
}

/// A scripting engine.
pub struct Engine<'a> {
    vm: Vm<'a, SmallPrimitiveSet<Device, VoidFileSystem, VoidProcessContext, VoidClock>>,
}

impl<'a> Engine<'a> {
    /// Creates a scripting engine.
    pub fn new(heap: &'a mut [Value]) -> Result<Self, Error> {
        Ok(Self {
            vm: Vm::new(
                heap,
                SmallPrimitiveSet::new(
                    Default::default(),
                    Default::default(),
                    Default::default(),
                    Default::default(),
                ),
            )?,
        })
    }

    /// Runs a module.
    pub fn run<'m>(&mut self, module: &'m impl Module<'m>) -> Result<(), Error> {
        self.vm.initialize(module.bytecode().into_iter().copied())
    }
}
