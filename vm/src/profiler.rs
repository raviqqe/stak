use crate::{Cons, Error, Memory, memory::Heap};

/// A profiler.
pub trait Profiler {
    /// Profiles a call.
    fn profile_call<T: Heap>(
        &mut self,
        memory: &Memory<T>,
        call_code: Cons,
        r#return: bool,
    ) -> Result<(), Error>;

    /// Profiles a return.
    fn profile_return<T: Heap>(&mut self, memory: &Memory<T>) -> Result<(), Error>;

    /// Profiles a call.
    fn profile_event(&mut self, name: &str) -> Result<(), Error>;
}
