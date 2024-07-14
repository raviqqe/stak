use crate::{read_file_size, validate};
use core::{ffi::CStr, ptr::null_mut, slice};

/// A mmap.
pub struct Mmap {
    ptr: *mut u8,
    len: usize,
}

impl Mmap {
    /// Creates a mmap opening a file at a path.
    pub fn new(path: &CStr) -> Self {
        let len = read_file_size(path);

        Self {
            ptr: unsafe {
                libc::mmap(
                    null_mut(),
                    len,
                    libc::PROT_READ,
                    libc::MAP_PRIVATE,
                    // spell-checker: disable-next-line
                    libc::open(path.as_ptr(), libc::O_RDONLY),
                    0,
                )
            } as _,
            len,
        }
    }

    /// Returns a slice of bytes.
    pub fn as_slice(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.ptr, self.len) }
    }
}

impl Drop for Mmap {
    fn drop(&mut self) {
        unsafe {
            validate(libc::munmap(self.ptr as _, self.len));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_file() {
        Mmap::new(c"src/lib.rs");
    }
}
