mod error;
#[cfg(feature = "libc")]
mod libc;
mod memory;
#[cfg(feature = "std")]
mod os;
mod utility;
mod void;

pub use self::error::FileError;
use core::error::Error;
#[cfg(feature = "libc")]
pub use libc::LibcFileSystem;
pub use memory::MemoryFileSystem;
#[cfg(feature = "std")]
pub use os::OsFileSystem;
use stak_vm::{Memory, Value};
pub use void::VoidFileSystem;

/// A file descriptor.
pub type FileDescriptor = usize;

/// A file system.
pub trait FileSystem {
    /// A path.
    type Path: ?Sized;

    /// A path buffer.
    type PathBuf: AsRef<Self::Path>;

    /// An error.
    type Error: Error;

    /// Opens a file and returns its descriptor.
    fn open(&mut self, path: &Self::Path, output: bool) -> Result<FileDescriptor, Self::Error>;

    /// Closes a file.
    fn close(&mut self, descriptor: FileDescriptor) -> Result<(), Self::Error>;

    /// Reads a file.
    fn read(&mut self, descriptor: FileDescriptor) -> Result<u8, Self::Error>;

    /// Writes a file.
    fn write(&mut self, descriptor: FileDescriptor, byte: u8) -> Result<(), Self::Error>;

    /// Flushes a file.
    fn flush(&mut self, descriptor: FileDescriptor) -> Result<(), Self::Error>;

    /// Deletes a file.
    fn delete(&mut self, path: &Self::Path) -> Result<(), Self::Error>;

    /// Checks if a file exists.
    fn exists(&self, path: &Self::Path) -> Result<bool, Self::Error>;

    /// Decodes a path.
    fn decode_path(memory: &Memory, list: Value) -> Result<Self::PathBuf, Self::Error>;
}
