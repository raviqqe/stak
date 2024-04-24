use crate::{Cons, PrimitiveSet, Vm};

pub trait Profiler<T: PrimitiveSet> {
    fn profile_call(&mut self, vm: &Vm<T>, call_code: Cons);

    fn profile_return(&mut self, vm: &Vm<T>);
}
