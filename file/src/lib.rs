//! File systems.

#![no_std]

#[cfg(test)]
extern crate std;

mod error;
mod file_system;
#[cfg(feature = "libc")]
mod libc;
mod void;

pub use error::Error;
pub use file_system::FileSystem;
#[cfg(feature = "libc")]
pub use libc::LibcFileSystem;
pub use void::VoidFileSystem;

pub type FileDescriptor = usize;
pub type OpenFlagSet = u32;
