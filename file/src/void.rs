use crate::{Error, FileDescriptor, FileSystem, OpenFlagSet};

pub struct VoidFileSystem {}

impl FileSystem for VoidFileSystem {
    type Error = Error;

    fn open(&self, _: &[u8], _: OpenFlagSet) -> Result<FileDescriptor, Self::Error> {
        Err(Error::Open)
    }

    fn read(&self, _: FileDescriptor) -> Result<u8, Self::Error> {
        Err(Error::Read)
    }

    fn write(&self, _: FileDescriptor, _: u8) -> Result<(), Self::Error> {
        Err(Error::Write)
    }
}
