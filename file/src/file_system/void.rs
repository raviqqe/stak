use crate::{FileDescriptor, FileError, FileSystem};
use stak_vm::{Memory, Value};

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
    type Path = [u8];
    type PathBuf = [u8; 0];
    type Error = FileError;

    fn open(&mut self, _: &Self::Path, _: bool) -> Result<FileDescriptor, Self::Error> {
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

    fn delete(&mut self, _: &Self::Path) -> Result<(), Self::Error> {
        Err(FileError::Delete)
    }

    fn exists(&self, _: &Self::Path) -> Result<bool, Self::Error> {
        Err(FileError::Exists)
    }

    fn decode_path(&self, _: &Memory, _: Value) -> Result<Self::PathBuf, Self::Error> {
        Err(FileError::PathDecode)
    }
}

impl Default for VoidFileSystem {
    fn default() -> Self {
        Self::new()
    }
}
