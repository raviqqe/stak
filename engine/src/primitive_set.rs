use crate::EngineError;
use any_fn::AnyFn;
use cfg_elif::item;
use stak_file::VoidFileSystem;
use stak_native::dynamic::DynamicPrimitiveSet;
use stak_process_context::VoidProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::{Memory, PrimitiveSet};

const DYNAMIC_PRIMITIVE_OFFSET: usize = 1000;

item::feature!(if ("std") {
    type Device = stak_device::StdioDevice;
} else if ("libc") {
    type Device = stak_device::LibcDevice;
} else {
    type Device = stak_device::VoidDevice;
});

/// A type check primitive set.
pub struct EnginePrimitiveSet<'a, 'b, const N: usize> {
    small: SmallPrimitiveSet<Device, VoidFileSystem, VoidProcessContext, VoidClock>,
    dynamic: DynamicPrimitiveSet<'a, 'b, N>,
}

impl<'a, 'b, const N: usize> EnginePrimitiveSet<'a, 'b, N> {
    /// Creates a primitive set.
    pub fn new(functions: &'a mut [AnyFn<'b>]) -> Self {
        Self {
            small: SmallPrimitiveSet::new(
                Default::default(),
                Default::default(),
                Default::default(),
                Default::default(),
            ),
            dynamic: DynamicPrimitiveSet::new(functions),
        }
    }
}

impl<'a, 'b, const N: usize> PrimitiveSet for EnginePrimitiveSet<'a, 'b, N> {
    type Error = EngineError;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        if primitive >= DYNAMIC_PRIMITIVE_OFFSET {
            self.dynamic
                .operate(memory, primitive - DYNAMIC_PRIMITIVE_OFFSET)?
        } else {
            self.small.operate(memory, primitive)?
        }

        Ok(())
    }
}
