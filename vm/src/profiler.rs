use crate::Cons;

pub trait Profiler {
    fn profile_call(&self, call_code: Cons);
    fn profile_return(&self);
}
