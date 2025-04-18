use core::{ffi::CStr, ptr::null_mut, slice};
use rustix::{
    fs::{self, Mode, OFlags},
    io,
    mm::{MapFlags, ProtFlags, mmap, munmap},
};

/// A mmap.
pub struct Mmap {
    ptr: *mut u8,
    len: usize,
}

impl Mmap {
    /// Creates a mmap opening a file at a path.
    pub fn new(path: &CStr) -> io::Result<Self> {
        let len = fs::stat(path)?.st_size as _;
        // spell-checker: disable-next-line
        let descriptor = fs::open(path, OFlags::RDONLY, Mode::RUSR)?;

        Ok(Self {
            // SAFETY: The passed pointer is null.
            ptr: unsafe {
                mmap(
                    null_mut(),
                    len,
                    ProtFlags::READ,
                    MapFlags::PRIVATE,
                    descriptor,
                    0,
                )?
            } as _,
            len,
        })
    }

    /// Returns a slice of bytes.
    pub const fn as_slice(&self) -> &[u8] {
        // SAFETY: `self.ptr` is guaranteed to have the length of `self.len`.
        unsafe { slice::from_raw_parts(self.ptr, self.len) }
    }
}

impl Drop for Mmap {
    fn drop(&mut self) {
        // SAFETY: We ensure that the `mmap` call succeeds.
        unsafe { munmap(self.ptr as _, self.len).unwrap() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_file() {
        Mmap::new(c"src/lib.rs").unwrap();
    }
}
