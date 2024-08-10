use crate::ProcessContext;
use alloc::{string::String, vec::Vec};
use std::{
    env::{args, vars},
    sync::LazyLock,
};

/// A process context provided by an OS.
pub struct OsProcessContext {
    arguments: LazyLock<Vec<String>>,
    environment_variables: LazyLock<Vec<(String, String)>>,
}

impl OsProcessContext {
    /// Creates a process context.
    pub fn new() -> Self {
        Self {
            arguments: LazyLock::new(|| args().collect()),
            environment_variables: LazyLock::new(|| vars().collect()),
        }
    }
}

impl ProcessContext for OsProcessContext {
    fn command_line_rev(&self) -> impl IntoIterator<Item = &str> {
        (*self.arguments).iter().map(AsRef::as_ref).rev()
    }

    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)> {
        (*self.environment_variables)
            .iter()
            .map(|(key, value)| (key.as_ref(), value.as_ref()))
    }
}

impl Default for OsProcessContext {
    fn default() -> Self {
        Self::new()
    }
}
