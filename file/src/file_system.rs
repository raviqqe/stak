use crate::{Error, FileDescriptor};
use core::fmt::Debug;

pub trait FileSystem {
    type Error: Debug;

    fn open(&self, path: &[u8]) -> Result<FileDescriptor, Error>;
    fn read(&self, descriptor: FileDescriptor) -> Result<u8, Error>;
    fn write(&self, descriptor: FileDescriptor, byte: u8) -> Result<(), Error>;
}
