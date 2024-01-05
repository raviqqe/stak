use crate::Device;
use std::io::{Error, Read, Write};

#[derive(Clone, Copy, Debug)]
pub struct ReadWriteDevice<I: Read, O: Write, E: Write> {
    input: I,
    output: O,
    error: E,
}

impl<I: Read, O: Write, E: Write> ReadWriteDevice<I, O, E> {
    /// Creates a device.
    pub fn new(input: I, output: O, error: E) -> Self {
        Self {
            input,
            output,
            error,
        }
    }

    /// Returns a reference to output.
    pub fn output(&self) -> &O {
        &self.output
    }

    /// Returns a reference to error.
    pub fn error(&self) -> &E {
        &self.error
    }
}

impl<I: Read, O: Write, E: Write> Device for ReadWriteDevice<I, O, E> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use std::io::empty;

    #[test]
    fn read() {
        let mut device = ReadWriteDevice::new([1, 2, 3].as_slice(), empty(), empty());

        assert_eq!(device.read().unwrap(), Some(1));
        assert_eq!(device.read().unwrap(), Some(2));
        assert_eq!(device.read().unwrap(), Some(3));
        assert_eq!(device.read().unwrap(), None);
    }

    #[test]
    fn write() {
        let mut device = ReadWriteDevice::new(empty(), vec![], empty());

        device.write(42).unwrap();

        assert_eq!(device.output().unwrap(), vec![42]);
    }
}
