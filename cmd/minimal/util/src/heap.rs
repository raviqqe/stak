use core::{mem::size_of, slice};

/// A memory block on a heap.
pub struct Heap<T> {
    ptr: *mut T,
    len: usize,
}

impl<T> Heap<T> {
    /// Creates a heap.
    pub fn new(len: usize) -> Self {
        Self {
            ptr: unsafe { libc::malloc(len * size_of::<T>()) } as _,
            len,
        }
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
