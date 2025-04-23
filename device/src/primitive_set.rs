mod error;
mod primitive;

pub use self::{error::PrimitiveError, primitive::Primitive};
use crate::Device;
use stak_vm::{Memory, Number, PrimitiveSet};

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

    fn write(
        &mut self,
        memory: &mut Memory,
        write: fn(&mut T, u8) -> Result<(), <T as Device>::Error>,
        error: PrimitiveError,
    ) -> Result<(), PrimitiveError> {
        let byte = memory.pop().assume_number().to_i64() as u8;
        write(&mut self.device, byte).map_err(|_| error)?;
        memory.push(memory.boolean(false).into())?;

        Ok(())
    }
}

impl<T: Device> PrimitiveSet for DevicePrimitiveSet<T> {
    type Error = PrimitiveError;

    async fn operate(
        &mut self,
        memory: &mut Memory<'_>,
        primitive: usize,
    ) -> Result<(), Self::Error> {
        match primitive {
            Primitive::READ => {
                let byte = self.device.read().map_err(|_| PrimitiveError::ReadInput)?;

                memory.push(byte.map_or_else(
                    || memory.boolean(false).into(),
                    |byte| Number::from_i64(byte as _).into(),
                ))?;
            }
            Primitive::WRITE => self.write(memory, Device::write, PrimitiveError::WriteOutput)?,
            Primitive::WRITE_ERROR => {
                self.write(memory, Device::write_error, PrimitiveError::WriteError)?
            }
            _ => return Err(stak_vm::Error::IllegalPrimitive.into()),
        }

        Ok(())
    }
}
