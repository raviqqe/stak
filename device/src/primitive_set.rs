mod error;

use self::error::Error;
use crate::Device;
use stak_vm::{Memory, Number, PrimitiveSet};

/// A primitive set for devices.
pub struct DevicePrimitiveSet<const READ: u8, const WRITE: u8, const WRITE_ERROR: u8, D: Device> {
    device: D,
}

impl<const READ: u8, const WRITE: u8, const WRITE_ERROR: u8, D: Device>
    DevicePrimitiveSet<READ, WRITE, WRITE_ERROR, D>
{
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

impl<const READ: u8, const WRITE: u8, const WRITE_ERROR: u8, D: Device> PrimitiveSet
    for DevicePrimitiveSet<READ, WRITE, WRITE_ERROR, D>
{
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: u8) -> Result<(), Self::Error> {
        if primitive == READ {
            let byte = self.device.read().map_err(|_| Error::ReadInput)?;

            memory.push(if let Some(byte) = byte {
                Number::from_i64(byte as _).into()
            } else {
                memory.boolean(false).into()
            })?;
        } else if primitive == WRITE {
            self.write(memory, Device::write, Error::WriteOutput)?;
        } else if primitive == WRITE_ERROR {
            self.write(memory, Device::write_error, Error::WriteError)?;
        } else {
            return Err(Error::Illegal);
        }

        Ok(())
    }
}
