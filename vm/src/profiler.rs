use crate::{Cons, Memory, PrimitiveSet};

/// A profiler.
pub trait Profiler<T: PrimitiveSet> {
    fn profile_call(&mut self, memory: &Memory, call_code: Cons, r#return: bool);

    fn profile_return(&mut self, memory: &Memory);
}
