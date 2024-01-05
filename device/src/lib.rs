//! Devices to handle I/O.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod device;
mod fixed_buffer;
#[cfg(feature = "std")]
mod stdio;

pub use device::Device;
pub use fixed_buffer::FixedBufferDevice;
use std::io::{Error, Read, Write};
#[cfg(feature = "std")]
pub use stdio::StdioDevice;

#[derive(Debug, Default)]
pub struct NormalDevice<I: Read, O: Write, E: Write> {
    input: I,
    output: O,
    error: E,
}

impl<I: Read, O: Write, E: Write> NormalDevice<I, O, E> {
    /// Creates a device.
    pub fn new(input: I, output: O, error: E) -> Self {
        Self {
            input,
            output,
            error,
        }
    }
}

impl<I: Read, O: Write, E: Write> Device for NormalDevice<I, O, E> {
    type Error = Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        let mut buffer = [0u8; 1];

        let count = self.input.read(&mut buffer)?;

        Ok(if count == 0 { None } else { Some(buffer[0]) })
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.output.write_all(&[byte])?;

        Ok(())
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.error.write_all(&[byte])?;

        Ok(())
    }
}
