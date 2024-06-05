//! File systems.

#![no_std]

#[cfg(test)]
extern crate std;

mod error;
mod flag;
#[cfg(feature = "libc")]
mod libc;
mod system;
mod void;

pub use error::Error;
#[cfg(feature = "libc")]
pub use libc::LibcFileSystem;
pub use system::FileSystem;
pub use void::VoidFileSystem;

pub type FileDescriptor = usize;
pub type OpenFlagSet = u32;
