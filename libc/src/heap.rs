use alloc::alloc::{alloc, dealloc};
use core::{alloc::Layout, ptr::write, slice};

/// A memory block on a heap.
pub struct Heap<T> {
    ptr: *mut T,
    len: usize,
}

impl<T> Heap<T> {
    /// Creates a heap.
    ///
    /// # Panics
    ///
    /// Panics if `len` is zero or if an allocation fails.
    pub fn new(len: usize, default: impl Fn() -> T) -> Self {
        let mut this = Self {
            // SAFETY: We allow memory access only within `len`.
            ptr: unsafe { alloc(Layout::array::<T>(len).unwrap()) } as _,
            len,
        };

        for x in this.as_slice_mut() {
            // SAFETY: `x` is not initialized yet.
            unsafe { write(x, default()) };
        }

        this
    }

    /// Returns a slice.
    pub const fn as_slice(&mut self) -> &[T] {
        // SAFETY: `self.ptr` has the length of `self.len`.
        unsafe { slice::from_raw_parts(self.ptr as _, self.len) }
    }

    /// Returns a mutable slice.
    pub const fn as_slice_mut(&mut self) -> &mut [T] {
        // SAFETY: `self.ptr` has the length of `self.len`.
        unsafe { slice::from_raw_parts_mut(self.ptr as _, self.len) }
    }
}

impl<T> Drop for Heap<T> {
    fn drop(&mut self) {
        // SAFETY: The previous `malloc` call is guaranteed to have succeeded.
        unsafe { dealloc(self.ptr as _, Layout::new::<T>()) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new() {
        Heap::<usize>::new(42, || 42);
    }

    #[test]
    fn new_zero_sized() {
        Heap::<usize>::new(0, || 42);
    }
}
