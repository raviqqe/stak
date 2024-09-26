//! A primitive set for devices.

mod error;

pub use self::error::Error;
use crate::Device;
use stak_vm::{Memory, Number, PrimitiveSet};

/// A primitive set for devices.
pub struct DevicePrimitiveSet<D: Device> {
    device: D,
}

impl<D: Device> DevicePrimitiveSet<D> {
    /// Creates a primitive set.
    pub fn new(device: D) -> Self {
        Self { device }
    }

    /// Returns a reference to a device.
    pub fn device(&self) -> &D {
        &self.device
    }

    /// Returns a mutable reference to a device.
    pub fn device_mut(&mut self) -> &mut D {
        &mut self.device
    }

    fn write(
        &mut self,
        memory: &mut Memory,
        write: fn(&mut D, u8) -> Result<(), <D as Device>::Error>,
        error: Error,
    ) -> Result<(), Error> {
        let byte = memory.top().assume_number().to_i64() as u8;

        write(&mut self.device, byte).map_err(|_| error)
    }
}

impl<D: Device> PrimitiveSet for DevicePrimitiveSet<D> {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: u8) -> Result<(), Self::Error> {
        match primitive {
            0 => {
                let byte = self.device.read().map_err(|_| Error::ReadInput)?;

                memory.push(if let Some(byte) = byte {
                    Number::from_i64(byte as _).into()
                } else {
                    memory.boolean(false).into()
                })?;
            }
            1 => self.write(memory, Device::write, Error::WriteOutput)?,
            2 => self.write(memory, Device::write_error, Error::WriteError)?,
            _ => return Err(stak_vm::Error::IllegalPrimitive.into()),
        }

        Ok(())
    }
}
