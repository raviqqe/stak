use crate::read_file_size;
use core::{ffi::CStr, ptr::null_mut, slice};
use rustix::{
    fs::{self, Mode, OFlags},
    mm::{mmap, munmap, MapFlags, ProtFlags},
};

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
                mmap(
                    null_mut(),
                    len,
                    ProtFlags::READ,
                    MapFlags::PRIVATE,
                    // spell-checker: disable-next-line
                    fs::open(path, OFlags::RDONLY, Mode::RUSR).unwrap(),
                    0,
                )
                .unwrap()
            } as _,
            len,
        }
    }

    /// Returns a slice of bytes.
    pub const fn as_slice(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.ptr, self.len) }
    }
}

impl Drop for Mmap {
    fn drop(&mut self) {
        unsafe {
            munmap(self.ptr as _, self.len).unwrap();
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
