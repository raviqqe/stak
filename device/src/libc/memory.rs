use super::{error::Error, ReadBuffer, WriteBuffer};
use crate::Device;

pub struct MemoryDevice<'a> {
    stdin: ReadBuffer<'a>,
    stdout: WriteBuffer<'a>,
    stderr: WriteBuffer<'a>,
}

impl<'a> MemoryDevice<'a> {
    pub fn new(stdin: &'a [u8], stdout: &'a mut [u8], stderr: &'a mut [u8]) -> Self {
        Self {
            stdin: ReadBuffer::new(stdin),
            stdout: WriteBuffer::new(stdout),
            stderr: WriteBuffer::new(stderr),
        }
    }
}

impl<'a> Device for MemoryDevice<'a> {
    type Error = Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        Ok(self.stdin.read())
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        if self.stdout.write(byte) {
            Err(Error::Stdout)
        } else {
            Ok(())
        }
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        if self.stderr.write(byte) {
            Err(Error::Stderr)
        } else {
            Ok(())
        }
    }
}
