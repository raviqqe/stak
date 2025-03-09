use super::{Read, Write, error::LibcError};
use crate::Device;

/// A device composed of objects implementing `Read` and `Write` traits.
pub struct ReadWriteDevice<I: Read, O: Write, E: Write> {
    stdin: I,
    stdout: O,
    stderr: E,
}

impl<I: Read, O: Write, E: Write> ReadWriteDevice<I, O, E> {
    /// Creates a device.
    pub const fn new(stdin: I, stdout: O, stderr: E) -> Self {
        Self {
            stdin,
            stdout,
            stderr,
        }
    }

    /// Returns a stdin.
    pub const fn stdin(&self) -> &I {
        &self.stdin
    }

    /// Returns a stdout.
    pub const fn stdout(&self) -> &O {
        &self.stdout
    }

    /// Returns a stderr.
    pub const fn stderr(&self) -> &E {
        &self.stderr
    }
}

impl<I: Read, O: Write, E: Write> Device for ReadWriteDevice<I, O, E> {
    type Error = LibcError;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        self.stdin.read().map_err(|_| LibcError::Stdin)
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.stdout.write(byte).map_err(|_| LibcError::Stdout)
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.stderr.write(byte).map_err(|_| LibcError::Stderr)
    }
}
