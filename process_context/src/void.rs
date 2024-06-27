use crate::ProcessContext;
use core::iter::DoubleEndedIterator;

/// A void process context that provides no context information.
#[derive(Debug, Default)]
pub struct VoidProcessContext {}

impl VoidProcessContext {
    pub const fn new() -> Self {
        Self {}
    }
}

impl ProcessContext for VoidProcessContext {
    fn command_line(
        &self,
    ) -> impl IntoIterator<Item = &str, IntoIter = impl DoubleEndedIterator<Item = &str>> {
        []
    }

    fn environment_variables(
        &self,
    ) -> impl IntoIterator<Item = (&str, &str), IntoIter = impl DoubleEndedIterator<Item = (&str, &str)>>
    {
        []
    }
}