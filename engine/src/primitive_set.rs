use crate::EngineError;
use any_fn::AnyFn;
use cfg_elif::item;
use stak_dynamic::DynamicPrimitiveSet;
use stak_file::VoidFileSystem;
use stak_process_context::VoidProcessContext;
use stak_r7rs::SmallPrimitiveSet;
use stak_time::VoidClock;
use stak_vm::{Memory, PrimitiveSet};
use winter_maybe_async::{maybe_async, maybe_await};

const DYNAMIC_PRIMITIVE_OFFSET: usize = 1000;

item::feature!(if ("std") {
    type Device = stak_device::StdioDevice;
} else if ("libc") {
    type Device = stak_device::LibcDevice;
} else {
    type Device = stak_device::VoidDevice;
});

/// A type check primitive set.
pub struct EnginePrimitiveSet<'a, 'b> {
    small: SmallPrimitiveSet<Device, VoidFileSystem, VoidProcessContext, VoidClock>,
    dynamic: DynamicPrimitiveSet<'a, 'b>,
}

impl<'a, 'b> EnginePrimitiveSet<'a, 'b> {
    /// Creates a primitive set.
    pub fn new(functions: &'a mut [(&'a str, AnyFn<'b>)]) -> Self {
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

    pub(crate) const fn dynamic_mut(&mut self) -> &mut DynamicPrimitiveSet<'a, 'b> {
        &mut self.dynamic
    }
}

impl PrimitiveSet for EnginePrimitiveSet<'_, '_> {
    type Error = EngineError;

    #[maybe_async]
    fn operate(&mut self, memory: &mut Memory<'_>, primitive: usize) -> Result<(), Self::Error> {
        if primitive >= DYNAMIC_PRIMITIVE_OFFSET {
            maybe_await!(
                self.dynamic
                    .operate(memory, primitive - DYNAMIC_PRIMITIVE_OFFSET)
            )?
        } else {
            maybe_await!(self.small.operate(memory, primitive))?
        }

        Ok(())
    }
}
