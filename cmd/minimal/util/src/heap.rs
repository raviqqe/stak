use core::{mem::size_of, slice};

pub struct Heap<T> {
    ptr: *mut T,
    len: usize,
}

impl<T> Heap<T> {
    /// Allocates a memory block on heap.
    pub fn new(len: usize) -> Self {
        Self {
            ptr: unsafe { libc::malloc(len * size_of::<T>()) } as _,
            len,
        }
    }

    pub fn as_slice(&mut self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr as _, self.len) }
    }

    pub fn as_slice_mut(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr as _, self.len) }
    }
}
