use crate::read_file_size;
use core::{ptr::null_mut, slice};

pub struct Mmap {
    ptr: *mut u8,
    len: usize,
}

impl Mmap {
    /// Creates a new mmap opening a file at a path.
    ///
    /// # Safety
    ///
    /// A given path must be a valid path of a C string.
    pub unsafe fn new(path: *const i8) -> Self {
        let len = read_file_size(path);

        Self {
            ptr: libc::mmap(
                null_mut(),
                len,
                libc::PROT_READ,
                libc::MAP_PRIVATE,
                // spell-checker: disable-next-line
                libc::open(path, libc::O_RDONLY),
                0,
            ) as _,
            len,
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.ptr, self.len) }
    }
}

impl Drop for Mmap {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.ptr as _, self.len);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_file() {
        unsafe {
            Mmap::new(c"src/lib.rs" as *const _ as _);
        }
    }
}
