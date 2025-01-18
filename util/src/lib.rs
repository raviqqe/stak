//! Utilities around `libc`.

#![no_std]

mod heap;
mod mmap;

use core::ffi::CStr;
pub use heap::Heap;
pub use mmap::Mmap;

/// Reads a file size at a path.
pub fn read_file_size(path: &CStr) -> usize {
    unsafe {
        let file = libc::fopen(path.as_ptr(), c"rb" as *const _ as _);
        libc::fseek(file, 0, libc::SEEK_END);
        let size = libc::ftell(file) as _;
        libc::fclose(file);
        size
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_size() {
        assert!(read_file_size(c"src/lib.rs") > 0);
    }
}
