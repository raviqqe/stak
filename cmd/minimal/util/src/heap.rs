use core::{mem::align_of, ptr::write, slice};

/// A memory block on a heap.
pub struct Heap<T> {
    ptr: *mut T,
    len: usize,
}

impl<T> Heap<T> {
    /// Creates a heap.
    pub fn new(len: usize, default: impl Fn() -> T) -> Self {
        let mut this = Self {
            ptr: unsafe { libc::malloc(len * align_of::<T>()) } as _,
            len,
        };

        for x in this.as_slice_mut() {
            unsafe { write(x, default()) };
        }

        this
    }

    /// Returns a slice.
    pub fn as_slice(&mut self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr as _, self.len) }
    }

    /// Returns a mutable slice.
    pub fn as_slice_mut(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr as _, self.len) }
    }
}

impl<T> Drop for Heap<T> {
    fn drop(&mut self) {
        unsafe { libc::free(self.ptr as _) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_size() {
        Heap::<usize>::new(42, || 42);
    }
}
