mod error;
mod primitive;

pub use self::{error::PrimitiveError, primitive::Primitive};
use crate::Device;
use stak_vm::{Memory, Number, PrimitiveSet};
use winter_maybe_async::{maybe_async, maybe_await};

/// A primitive set for a device.
pub struct DevicePrimitiveSet<T: Device> {
    device: T,
}

impl<T: Device> DevicePrimitiveSet<T> {
    /// Creates a primitive set.
    pub const fn new(device: T) -> Self {
        Self { device }
    }

    /// Returns a reference to a device.
    pub const fn device(&self) -> &T {
        &self.device
    }

    /// Returns a mutable reference to a device.
    pub const fn device_mut(&mut self) -> &mut T {
        &mut self.device
    }
}

impl<T: Device> PrimitiveSet for DevicePrimitiveSet<T> {
    type Error = PrimitiveError;

    #[maybe_async]
    fn operate(&mut self, memory: &mut Memory<'_>, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            Primitive::READ => {
                let byte =
                    maybe_await!(self.device.read()).map_err(|_| PrimitiveError::ReadInput)?;

                memory.push(byte.map_or_else(
                    || memory.boolean(false).into(),
                    |byte| Number::from_i64(byte as _).into(),
                ))?;
            }
            Primitive::WRITE => {
                let byte = memory.pop().assume_number().to_i64() as u8;
                maybe_await!(self.device.write(byte)).map_err(|_| PrimitiveError::WriteOutput)?;
                memory.push(memory.boolean(false).into())?;
            }
            Primitive::WRITE_ERROR => {
                let byte = memory.pop().assume_number().to_i64() as u8;
                maybe_await!(self.device.write_error(byte))
                    .map_err(|_| PrimitiveError::WriteError)?;
                memory.push(memory.boolean(false).into())?;
            }
            _ => return Err(stak_vm::Error::IllegalPrimitive.into()),
        }

        Ok(())
    }
}
