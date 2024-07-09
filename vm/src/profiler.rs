use crate::{Cons, Memory};

/// A profiler.
pub trait Profiler {
    /// Profiles a call.
    fn profile_call(&mut self, memory: &Memory, call_code: Cons, r#return: bool);

    /// Profiles a return.
    fn profile_return(&mut self, memory: &Memory);
}
