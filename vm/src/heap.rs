use crate::value::Value;

/// A heap memory.
pub trait Heap: AsRef<[Value]> + AsMut<[Value]> {}

impl<T: AsRef<[Value]> + AsMut<[Value]>> Heap for T {}
