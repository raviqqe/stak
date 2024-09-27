use crate::Time;
use alloc::{string::String, vec::Vec};
use std::{
    env::{args, vars},
    sync::LazyLock,
};

/// A time provided by an OS.
pub struct OsTime {
    arguments: LazyLock<Vec<String>>,
    environment_variables: LazyLock<Vec<(String, String)>>,
}

impl OsTime {
    /// Creates a time.
    pub fn new() -> Self {
        Self {
            arguments: LazyLock::new(|| args().collect()),
            environment_variables: LazyLock::new(|| vars().collect()),
        }
    }
}

impl Time for OsTime {
    fn command_line_rev(&self) -> impl IntoIterator<Item = &str> {
        (*self.arguments).iter().map(AsRef::as_ref).rev()
    }

    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)> {
        (*self.environment_variables)
            .iter()
            .map(|(key, value)| (key.as_ref(), value.as_ref()))
    }
}

impl Default for OsTime {
    fn default() -> Self {
        Self::new()
    }
}
