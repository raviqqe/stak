use cfg_if::cfg_if;
use stak_file::VoidFileSystem;
use stak_process_context::VoidProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::{Value, Vm};

cfg_if!(
    if #[cfg(feature = "std")] {
        type Device = stak_device::StdioDevice;
    } else {
        type Device<'a> = stak_device::FixedBufferDevice::<'a, 0, 0>;
    }
);

/// A scripting engine.
pub struct Engine<'a> {
    vm: Vm<'a, SmallPrimitiveSet<Device<'a>, VoidFileSystem, VoidProcessContext, VoidClock>>,
}

impl<'a> Engine<'a> {
    /// Creates a scripting engine.
    pub fn new(heap: &'a mut [Value]) -> Result<Self, stak_vm::Error> {
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
}
