use crate::{Cons, PrimitiveSet, Vm};

/// A profiler.
pub trait Profiler<T: PrimitiveSet> {
    fn profile_call(&mut self, vm: &Vm<T>, call_code: Cons, r#return: bool);

    fn profile_return(&mut self, vm: &Vm<T>);
}
