//! Devices based on libc.

mod buffer;
mod buffered_reader;
mod device;
mod error;
mod line_writer;
mod read;
mod sink;
mod source;
mod stdio;
mod write;

pub use buffer::{Buffer, BufferMut};
pub use device::ReadWriteDevice;
pub use error::LibcError;
pub use read::Read;
pub use stdio::{Stderr, Stdin, Stdout};
pub use write::Write;
