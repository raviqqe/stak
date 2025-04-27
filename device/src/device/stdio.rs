use winter_maybe_async::maybe_async;

use crate::{Device, ReadWriteDevice};
use std::io::{Error, Stderr, Stdin, Stdout, stderr, stdin, stdout};

/// A standard I/O device of a current process.
#[derive(Debug)]
pub struct StdioDevice {
    device: ReadWriteDevice<Stdin, Stdout, Stderr>,
}

impl StdioDevice {
    /// Creates a device.
    pub fn new() -> Self {
        Self {
            device: ReadWriteDevice::new(stdin(), stdout(), stderr()),
        }
    }
}

impl Device for StdioDevice {
    type Error = Error;

    #[maybe_async]
    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        self.device.read()
    }

    #[maybe_async]
    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.device.write(byte)
    }

    #[maybe_async]
    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.device.write_error(byte)
    }
}

impl Default for StdioDevice {
    fn default() -> Self {
        Self::new()
    }
}
