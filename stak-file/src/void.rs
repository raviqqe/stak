use crate::{Error, FileDescriptor, FileSystem};

pub struct VoidFileSystem {}

impl FileSystem for VoidFileSystem {
    type Error = Error;

    fn open(&self, path: &[u8]) -> Result<FileDescriptor, Self::Error> {
        Err(Error::Open)
    }

    fn read(&self, descriptor: FileDescriptor) -> Result<u8, Self::Error> {
        Err(Error::Read)
    }

    fn write(&self, descriptor: FileDescriptor, byte: u8) -> Result<(), Self::Error> {
        Err(Error::Write)
    }
}
