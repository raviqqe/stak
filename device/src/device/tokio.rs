use crate::Device;
use tokio::io::{AsyncReadExt, Error,AsyncWriteExt, Stderr, Stdin, Stdout, stderr, stdin, stdout};

/// A standard I/O device of a current process.
#[derive(Debug)]
pub struct TokioDevice {
    // TODO Add an async context field.
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
        let _future = self.stdin.read_u8();

        todo!("store future into async context")
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        let _future = self.stdout.write_u8(byte);

        todo!("store future into async context")
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        let _future = self.stderr.write_u8(byte);

        todo!("store future into async context")
    }
}

impl Default for TokioDevice {
    fn default() -> Self {
        Self::new()
    }
}
