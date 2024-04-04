use super::{error::Error, Read, Write};
use crate::Device;

pub struct ReadWriteDevice<I: Read, O: Write, E: Write> {
    stdin: I,
    stdout: O,
    stderr: E,
}

impl<I: Read, O: Write, E: Write> ReadWriteDevice<I, O, E> {
    pub fn new(stdin: I, stdout: O, stderr: E) -> Self {
        Self {
            stdin,
            stdout,
            stderr,
        }
    }

    pub fn stdin(&self) -> &I {
        &self.stdin
    }

    pub fn stdout(&self) -> &O {
        &self.stdout
    }

    pub fn stderr(&self) -> &E {
        &self.stderr
    }
}

impl<I: Read, O: Write, E: Write> Device for ReadWriteDevice<I, O, E> {
    type Error = Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        self.stdin.read().map_err(|_| Error::Stdin)
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.stdout.write(byte).map_err(|_| Error::Stdout)
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        self.stderr.write(byte).map_err(|_| Error::Stderr)
    }
}
