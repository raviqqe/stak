use stak_device::Device;
use stak_file::VoidFileSystem;
use stak_native::dynamic::DynamicPrimitiveSet;
use stak_process_context::VoidProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::{Error, Memory, PrimitiveSet, Type};

const DYNAMIC_PRIMITIVE_COUNT: usize = 128;

// TODO Use `cfg-elif`.
cfg_if! {
    if #[cfg(feature = "std")] {
        type Device = stak_device::StdioDevice;
    } else {
        type Device = stak_device::VoidDevice;
    }
}

/// A type check primitive set.
#[derive(Debug, Default)]
pub struct ScriptPrimitiveSet<const N : usize> {
    small: SmallPrimitiveSet<Device, VoidFileSystem, VoidProcessContext, VoidClock>
    dynamic: DynamicPrimitiveSet<N>
}

impl ScriptPrimitiveSet {
    /// Creates a primitive set.
    pub fn new() -> Self {
        Self::default()
    }
}

impl PrimitiveSet for ScriptPrimitiveSet {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        if primitive > 1000 {
             self
                .small
                .operate(memory, primitive - Primitive::CURRENT_JIFFY)?
    } else {
             self
                .small
                .operate(memory, primitive - Primitive::CURRENT_JIFFY)?
        }

        Ok(())
    }
}
