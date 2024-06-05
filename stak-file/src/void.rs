use crate::{Error, FileDescriptor, FileSystem};

pub struct VoidFileSystem {}

impl FileSystem for VoidFileSystem {
    fn open(&self, path: &[u8]) -> Result<FileDescriptor, Error> {
        Err(Error::Open)
    }

    fn read(&self, descriptor: FileDescriptor) -> Result<u8, Error> {
        Err(Error::Read)
    }

    fn write(&self, descriptor: FileDescriptor, byte: u8) -> Result<(), Error> {
        Err(Error::Write)
    }
}
