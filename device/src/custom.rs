use crate::Device;
use std::io::{Error, Read, Write};

#[derive(Debug, Default)]
pub struct CustomDevice<I: Read, O: Write, E: Write> {
    input: I,
    output: O,
    error: E,
}

impl<I: Read, O: Write, E: Write> CustomDevice<I, O, E> {
    /// Creates a device.
    pub fn new(input: I, output: O, error: E) -> Self {
        Self {
            input,
            output,
            error,
        }
    }
}

impl<I: Read, O: Write, E: Write> Device for CustomDevice<I, O, E> {
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
