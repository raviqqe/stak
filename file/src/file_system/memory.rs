use crate::{FileDescriptor, FileError, FileSystem};

/// A read-only in-memory file system.
#[derive(Debug)]
pub struct MemoryFileSystem<'a> {
    files: &'a [(&'a [u8], &'a [u8])],
}

impl<'a> MemoryFileSystem<'a> {
    /// Creates a file system.
    pub const fn new(files: &'a [(&'a [u8], &'a [u8])]) -> Self {
        Self { files }
    }
}

impl FileSystem for MemoryFileSystem {
    type Error = FileError;

    fn open(&self, path: &[u8], output: bool) -> Result<FileDescriptor, Self::Error> {
        if output {
            return Err(FileError::Open);
        }

        Ok(0)
    }

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
