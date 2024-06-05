use crate::Error;
use core::fmt::Debug;

pub trait FileSystem {
    type Error: Debug;

    fn open(&self, path: &[u8]) -> Result<usize, Error>;
    fn read(&self, descriptor: usize) -> Result<u8, Error>;
    fn write(&self, descriptor: usize, byte: u8) -> Result<(), Error>;
}
