use crate::{FileDescriptor, FileError, FileSystem};

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
    type Error = FileError;

    fn open(&mut self, _: &[u8], _: bool) -> Result<FileDescriptor, Self::Error> {
        Err(FileError::Open)
    }

    fn close(&mut self, _: FileDescriptor) -> Result<(), Self::Error> {
        Err(FileError::Close)
    }

    fn read(&mut self, _: FileDescriptor) -> Result<u8, Self::Error> {
        Err(FileError::Read)
    }

    fn write(&mut self, _: FileDescriptor, _: u8) -> Result<(), Self::Error> {
        Err(FileError::Write)
    }

    fn delete(&mut self, _: &[u8]) -> Result<(), Self::Error> {
        Err(FileError::Delete)
    }

    fn exists(&self, _: &[u8]) -> Result<bool, Self::Error> {
        Err(FileError::Exists)
    }
}

impl Default for VoidFileSystem {
    fn default() -> Self {
        Self::new()
    }
}
