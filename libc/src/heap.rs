use alloc::alloc::{alloc, dealloc};
use core::{
    alloc::Layout,
    ptr::{drop_in_place, write},
    slice,
};

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

        for x in this.as_mut() {
            // SAFETY: `x` is not initialized yet.
            unsafe { write(x, default()) };
        }

        this
    }
}

impl<T> Drop for Heap<T> {
    fn drop(&mut self) {
        for element in self.as_mut() {
            // SAFETY: Each `element` is valid.
            unsafe { drop_in_place(element) };
        }

        // SAFETY: The previous `malloc` call is guaranteed to have succeeded.
        unsafe { dealloc(self.ptr as _, Layout::array::<T>(self.len).unwrap()) }
    }
}

impl<T> AsRef<[T]> for Heap<T> {
    fn as_ref(&self) -> &[T] {
        // SAFETY: `self.ptr` has the length of `self.len`.
        unsafe { slice::from_raw_parts(self.ptr as _, self.len) }
    }
}

impl<T> AsMut<[T]> for Heap<T> {
    fn as_mut(&mut self) -> &mut [T] {
        // SAFETY: `self.ptr` has the length of `self.len`.
        unsafe { slice::from_raw_parts_mut(self.ptr as _, self.len) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::sync::atomic::{AtomicUsize, Ordering};

    static DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

    struct Foo {}

    impl Drop for Foo {
        fn drop(&mut self) {
            DROP_COUNT.fetch_add(1, Ordering::SeqCst);
        }
    }

    #[test]
    fn new() {
        Heap::<usize>::new(42, || 42);
    }

    #[test]
    fn new_zero_sized() {
        Heap::<usize>::new(0, || 42);
    }

    #[test]
    fn drop_elements() {
        let heap = Heap::new(42, || Foo {});

        drop(heap);

        assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 42);
    }
}
