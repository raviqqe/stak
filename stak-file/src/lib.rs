//! File systems.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod error;
mod file_system;
mod void;

pub use error::Error;
pub use file_system::FileSystem;
pub use void::VoidFileSystem;

pub type FileDescriptor = usize;
