pub trait ModIndex<T> {
    fn at(&self, index: usize) -> &T;
    fn at_mut(&mut self, index: usize) -> &mut T;
}

fn calculate_slice_index<T>(slice: &[T], index: usize) -> usize {
    if slice.is_empty() {
        0
    } else {
        index % slice.len()
    }
}

impl<T> ModIndex<T> for [T] {
    fn at(&self, index: usize) -> &T {
        unsafe { &*self.as_ptr().add(calculate_slice_index(self, index)) }
    }

    fn at_mut(&mut self, index: usize) -> &mut T {
        unsafe { &mut *self.as_mut_ptr().add(calculate_slice_index(self, index)) }
    }
}
