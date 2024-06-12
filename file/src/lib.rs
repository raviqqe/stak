//! File systems.

#![no_std]

#[cfg(test)]
extern crate std;

mod error;
#[cfg(feature = "libc")]
mod libc;
mod system;
mod void;

pub use error::Error;
#[cfg(feature = "libc")]
pub use libc::LibcFileSystem;
pub use system::FileSystem;
pub use void::VoidFileSystem;

/// A file descriptor.
pub type FileDescriptor = usize;
