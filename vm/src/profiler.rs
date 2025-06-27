use crate::{Cons, Error, Memory};

/// A profiler.
pub trait Profiler {
    /// Profiles a call.
    fn profile_call(
        &mut self,
        memory: &Memory,
        call_code: Cons,
        r#return: bool,
    ) -> Result<(), Error>;

    /// Profiles a return.
    fn profile_return(&mut self, memory: &Memory) -> Result<(), Error>;

    /// Profiles a call.
    fn profile_event(&mut self, name: &str) -> Result<(), Error>;
}
