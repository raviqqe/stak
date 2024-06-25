//! File systems.

#![no_std]

#[cfg(any(feature = "std", test))]
extern crate std;

mod error;
#[cfg(feature = "libc")]
mod libc;
#[cfg(any(feature = "std"))]
mod os;
mod system;
mod void;

pub use error::Error;
#[cfg(feature = "libc")]
pub use libc::LibcFileSystem;
pub use system::FileSystem;
pub use void::VoidFileSystem;

/// A file descriptor.
pub type FileDescriptor = usize;
