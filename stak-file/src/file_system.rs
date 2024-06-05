use crate::Error;

pub trait FileSystem {
    type Error: Debug = Error;

    fn open(&self, path: &[u8]) -> Result<usize, Error>;
    fn read(&self, descriptor: usize) -> Result<u8, Error>;
    fn write(&self, descriptor: usize, byte: u8) -> Result<(), Error>;
}
