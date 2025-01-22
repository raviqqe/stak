use stak_device::Device;
use stak_file::VoidFileSystem;
use stak_process_context::VoidProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::{Error, Memory, PrimitiveSet, Type};

/// A type check primitive set.
#[derive(Debug, Default)]
pub struct ScriptPrimitiveSet {
    small: SmallPrimitiveSet<Device, VoidFileSystem, VoidProcessContext, VoidClock>
    dynamic: DynamicPrimitiveSet
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
