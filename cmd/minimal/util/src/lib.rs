//! Utilities around `libc`.

#![no_std]

mod mmap;
mod validate;

use core::{ffi::CStr, mem::size_of, slice};
pub use mmap::Mmap;
pub use validate::validate;

/// Allocates a memory block on heap.
pub fn allocate_heap<T>(size: usize) -> &'static mut [T] {
    unsafe { slice::from_raw_parts_mut(libc::malloc(size * size_of::<T>()) as _, size) }
}

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
