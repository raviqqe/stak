use crate::ProcessContext;
use alloc::{string::String, vec::Vec};
use once_cell::unsync::Lazy;
use std::env::{args, vars};

/// A process context provided by an OS.
pub struct OsProcessContext {
    arguments: Lazy<Vec<String>>,
    environment_variables: Lazy<Vec<(String, String)>>,
}

impl OsProcessContext {
    pub fn new() -> Self {
        Self {
            arguments: Lazy::new(|| args().collect()),
            environment_variables: Lazy::new(|| vars().collect()),
        }
    }
}

impl ProcessContext for OsProcessContext {
    fn command_line(&self) -> impl IntoIterator<Item = &str> {
        (*self.arguments).iter().map(AsRef::as_ref)
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
