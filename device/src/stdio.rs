use crate::Device;
use std::io::{stderr, stdin, stdout, Error, Read, Write};

#[derive(Debug, Default)]
pub struct StdioDevice {}

impl StdioDevice {
    pub fn new() -> Self {
        Self {}
    }
}

impl Device for StdioDevice {
    type Error = Error;

    fn read(&mut self) -> Result<Option<u8>, Self::Error> {
        let mut buffer = [0u8; 1];

        let count = stdin().read(&mut buffer)?;

        Ok(if count == 0 { None } else { Some(buffer[0]) })
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        stdout().write_all(&[byte])?;

        Ok(())
    }

    fn write_error(&mut self, byte: u8) -> Result<(), Self::Error> {
        stderr().write_all(&[byte])?;

        Ok(())
    }
}
