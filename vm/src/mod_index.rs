pub trait ModIndex<T> {
    fn at(&self, index: usize) -> &T;
    fn at_mut(&mut self, index: usize) -> &mut T;
}

impl<T> ModIndex<T> for [T] {
    fn at(&self, index: usize) -> &T {
        unsafe { &*self.as_ptr().add(index % slice.len()) }
    }

    fn at_mut(&mut self, index: usize) -> &mut T {
        unsafe { &mut *self.as_mut_ptr().add(index % slice.len()) }
    }
}
