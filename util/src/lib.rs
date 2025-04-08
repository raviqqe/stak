//! Utilities around `libc`.

#![no_std]

mod heap;
mod mmap;

use core::ffi::CStr;
pub use heap::Heap;
pub use mmap::Mmap;
use rustix::{
    fs::{self, Mode, OFlags, SeekFrom},
    io,
};

/// Reads a file size at a path.
pub fn read_file_size(path: &CStr) -> io::Result<usize> {
    let descriptor = fs::open(path, OFlags::RDONLY, Mode::RUSR)?;
    fs::seek(&descriptor, SeekFrom::End(0))?;
    Ok(fs::tell(descriptor)? as _)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_size() {
        assert!(read_file_size(c"src/lib.rs") > 0);
    }
}
