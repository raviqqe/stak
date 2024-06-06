use crate::{Error, FileDescriptor, FileSystem};

/// A file system that does nothing and fails every operation.
#[derive(Debug)]
pub struct VoidFileSystem {}

impl VoidFileSystem {
    /// Creates a file system.
    pub const fn new() -> Self {
        Self {}
    }
}

impl FileSystem for VoidFileSystem {
    type Error = Error;

    fn open(&self, _: &[u8], _: bool) -> Result<FileDescriptor, Self::Error> {
        Err(Error::Open)
    }

    fn close(&self, _: FileDescriptor) -> Result<(), Self::Error> {
        Err(Error::Close)
    }

    fn read(&self, _: FileDescriptor) -> Result<u8, Self::Error> {
        Err(Error::Read)
    }

    fn write(&self, _: FileDescriptor, _: u8) -> Result<(), Self::Error> {
        Err(Error::Write)
    }
}

impl Default for VoidFileSystem {
    fn default() -> Self {
        Self::new()
    }
}
