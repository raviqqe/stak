use crate::ProcessContext;
use alloc::{string::String, vec::Vec};
use std::{
    env::{args, vars},
    sync::LazyLock,
};

/// A process context provided by an operating system.
pub struct OsProcessContext<const N: usize = 0> {
    arguments: LazyLock<Vec<String>>,
    environment_variables: LazyLock<Vec<(String, String)>>,
}

impl<const N: usize> OsProcessContext<N> {
    /// Creates a process context.
    pub fn new() -> Self {
        Self {
            arguments: LazyLock::new(|| args().skip(N).collect()),
            environment_variables: LazyLock::new(|| vars().collect()),
        }
    }
}

impl<const N: usize> ProcessContext for OsProcessContext<N> {
    fn command_line_rev(&self) -> impl IntoIterator<Item = &str> {
        (*self.arguments).iter().map(AsRef::as_ref).rev()
    }

    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)> {
        (*self.environment_variables)
            .iter()
            .map(|(key, value)| (key.as_ref(), value.as_ref()))
    }
}

impl<const N: usize> Default for OsProcessContext<N> {
    fn default() -> Self {
        Self::new()
    }
}
