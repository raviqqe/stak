use crate::Device;
use std::io::{stdin, stdout, Error, Read, Write};

#[derive(Debug, Default)]
pub struct StdioDevice {}

impl StdioDevice {
    pub fn new() -> Self {
        Self {}
    }
}

impl Device for StdioDevice {
    type Error = Error;

    fn read(&mut self) -> Result<u8, Self::Error> {
        let mut buffer = [0u8; 1];

        stdin().read_exact(&mut buffer)?;

        Ok(buffer[0])
    }

    fn write(&mut self, byte: u8) -> Result<(), Self::Error> {
        stdout().write_all(&[byte])?;

        Ok(())
    }
}
