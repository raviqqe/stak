use crate::{FileDescriptor, FileError, FileSystem};

/// A read-only in-memory file system.
#[derive(Debug)]
pub struct MemoryFileSystem {}

impl MemoryFileSystem {
    /// Creates a file system.
    pub const fn new() -> Self {
        Self {}
    }
}

impl FileSystem for MemoryFileSystem {
    type Error = FileError;

    fn open(&self, path: &[u8], output: bool) -> Result<FileDescriptor, Self::Error> {}

    fn close(&self, _: FileDescriptor) -> Result<(), Self::Error> {
        Err(FileError::Close)
    }

    fn read(&self, path: FileDescriptor) -> Result<u8, Self::Error> {
        Err(FileError::Read)
    }

    fn write(&self, _: FileDescriptor, _: u8) -> Result<(), Self::Error> {
        Err(FileError::Write)
    }

    fn delete(&self, _: &[u8]) -> Result<(), Self::Error> {
        Err(FileError::Delete)
    }

    fn exists(&self, path: &[u8]) -> Result<bool, Self::Error> {
        Err(FileError::Exists)
    }
}
