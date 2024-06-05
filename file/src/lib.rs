//! File systems.

#![no_std]

mod error;
mod file_system;
#[cfg(feature = "libc")]
mod libc;
mod void;

pub use error::Error;
pub use file_system::FileSystem;
pub use void::VoidFileSystem;

pub type FileDescriptor = usize;
