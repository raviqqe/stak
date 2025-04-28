use crate::Device;
use std::io::{Error, Read, Write};
use winter_maybe_async::maybe_async;

/// A device based on [`Read`](Read) and [`Write`](Write) traits.
#[derive(Clone, Copy, Debug)]
pub struct ReadWriteDevice<I: Read, O: Write, E: Write> {
    input: I,
    output: O,
    error: E,
}

impl<I: Read, O: Write, E: Write> ReadWriteDevice<I, O, E> {
    /// Creates a device.
    pub const fn new(input: I, output: O, error: E) -> Self {
        Self {
            input,
            output,
            error,
        }
    }

    /// Returns a reference to output.
    pub const fn output(&self) -> &O {
        &self.output
    }

    /// Returns a reference to error.
    pub const fn error(&self) -> &E {
        &self.error
    }
}

impl<I: Read, O: Write, E: Write> Device for ReadWriteDevice<I, O, E> {
    type Error = Error;

    #[maybe_async]
    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        let mut buffer = [0u8; 1];

        let count = self.input.read(&mut buffer)?;

        Ok(if count == 0 { None } else { Some(buffer[0]) })
    }

    #[maybe_async]
    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.output.write_all(&[byte])?;

        Ok(())
    }

    #[maybe_async]
    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.error.write_all(&[byte])?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use stak_util::block_on;
    use std::io::empty;

    #[test]
    fn read() {
        let mut device = ReadWriteDevice::new([1, 2, 3].as_slice(), empty(), empty());

        assert_eq!(block_on!(device.read()).unwrap(), Some(1));
        assert_eq!(block_on!(device.read()).unwrap(), Some(2));
        assert_eq!(block_on!(device.read()).unwrap(), Some(3));
        assert_eq!(block_on!(device.read()).unwrap(), None);
    }

    #[test]
    fn write() {
        let mut device = ReadWriteDevice::new(empty(), vec![], empty());

        block_on!(device.write(1)).unwrap();
        block_on!(device.write(2)).unwrap();
        block_on!(device.write(3)).unwrap();

        assert_eq!(device.output(), &[1, 2, 3]);
    }

    #[test]
    fn write_error() {
        let mut device = ReadWriteDevice::new(empty(), empty(), vec![]);

        block_on!(device.write_error(1)).unwrap();
        block_on!(device.write_error(2)).unwrap();
        block_on!(device.write_error(3)).unwrap();

        assert_eq!(device.error(), &[1, 2, 3]);
    }
}
