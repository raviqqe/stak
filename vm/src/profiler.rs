use crate::{Cons, Memory};

/// A profiler.
pub trait Profiler {
    fn profile_call(&mut self, memory: &Memory, call_code: Cons, r#return: bool);

    fn profile_return(&mut self, memory: &Memory);
}
