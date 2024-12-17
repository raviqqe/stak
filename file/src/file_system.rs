mod error;
#[doc(cfg(feature = "libc"))]
#[cfg(feature = "libc")]
mod libc;
#[doc(cfg(feature = "std"))]
#[cfg(feature = "std")]
mod os;
mod void;

pub use self::error::FileError;
use core::error::Error;
#[cfg(feature = "libc")]
pub use libc::LibcFileSystem;
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
    fn open(&self, path: &[u8], output: bool) -> Result<FileDescriptor, Self::Error>;

    /// Closes a file.
    fn close(&self, descriptor: FileDescriptor) -> Result<(), Self::Error>;

    /// Reads a file.
    fn read(&self, descriptor: FileDescriptor) -> Result<u8, Self::Error>;

    /// Writes a file.
    fn write(&self, descriptor: FileDescriptor, byte: u8) -> Result<(), Self::Error>;

    /// Deletes a file.
    fn delete(&self, path: &[u8]) -> Result<(), Self::Error>;

    /// Checks if a file exists.
    fn exists(&self, path: &[u8]) -> Result<bool, Self::Error>;
}
