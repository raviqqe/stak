mod error;
#[cfg(feature = "libc")]
mod libc;
mod memory;
#[cfg(feature = "std")]
mod os;
mod void;

pub use self::error::FileError;
use core::error::Error;
#[cfg(feature = "libc")]
pub use libc::LibcFileSystem;
pub use memory::MemoryFileSystem;
#[cfg(feature = "std")]
pub use os::OsFileSystem;
pub use void::VoidFileSystem;

/// A file descriptor.
pub type FileDescriptor = usize;

/// A file system.
pub trait FileSystem {
    /// An error.
    type Error: Error;

    /// Opens a file and returns its descriptor.
    fn open(&mut self, path: &[u8], output: bool) -> Result<FileDescriptor, Self::Error>;

    /// Closes a file.
    fn close(&mut self, descriptor: FileDescriptor) -> Result<(), Self::Error>;

    /// Reads a file.
    fn read(&mut self, descriptor: FileDescriptor) -> Result<u8, Self::Error>;

    /// Writes a file.
    fn write(&mut self, descriptor: FileDescriptor, byte: u8) -> Result<(), Self::Error>;

    /// Deletes a file.
    fn delete(&mut self, path: &[u8]) -> Result<(), Self::Error>;

    /// Checks if a file exists.
    fn exists(&self, path: &[u8]) -> Result<bool, Self::Error>;
}
