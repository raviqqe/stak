use crate::{Error, FileDescriptor, OpenFlagSet};
use core::fmt::Debug;

/// A file system.
pub trait FileSystem {
    type Error: Debug;

    /// Opens a file.
    fn open(&self, path: &[u8], flags: OpenFlagSet) -> Result<FileDescriptor, Error>;

    /// Closes a file.
    fn close(&self, descriptor: FileDescriptor) -> Result<(), Error>;

    /// Reads a file.
    fn read(&self, descriptor: FileDescriptor) -> Result<u8, Error>;

    /// Writes a file.
    fn write(&self, descriptor: FileDescriptor, byte: u8) -> Result<(), Error>;
}
