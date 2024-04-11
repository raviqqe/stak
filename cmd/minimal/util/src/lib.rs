//! Utilities around `libc`.

#![no_std]

mod mmap;
mod validate;

use core::{mem::size_of, slice};
pub use mmap::Mmap;
pub use validate::validate;

/// Allocates a memory block on heap.
pub fn allocate_heap<T>(size: usize) -> &'static mut [T] {
    unsafe { slice::from_raw_parts_mut(libc::malloc(size * size_of::<T>()) as _, size) }
}

/// Reads a file size at a path.
///
/// # Safety
///
/// A given path must be a valid path of a C string.
pub unsafe fn read_file_size(path: *const i8) -> usize {
    let file = libc::fopen(path, c"rb" as *const _ as _);
    libc::fseek(file, 0, libc::SEEK_END);
    let size = libc::ftell(file) as _;
    libc::fclose(file);
    size
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_size() {
        assert!(unsafe { read_file_size(c"src/lib.rs" as *const _ as *const i8) } > 0);
    }
}
