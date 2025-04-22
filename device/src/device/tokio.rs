use crate::Device;
use tokio::io::{Error, Stderr, Stdin, Stdout, stderr, stdin, stdout};

/// A standard I/O device of a current process.
#[derive(Debug)]
pub struct TokioDevice {
    stdin: Stdin,
    stdout: Stdout,
    stderr: Stderr,
}

impl TokioDevice {
    /// Creates a device.
    pub fn new() -> Self {
        Self {
            stdin: stdin(),
            stdout: stdout(),
            stderr: stderr(),
        }
    }
}

impl Device for TokioDevice {
    type Error = Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        self.device.read()
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.device.write(byte)
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.device.write_error(byte)
    }
}

impl Default for TokioDevice {
    fn default() -> Self {
        Self::new()
    }
}
