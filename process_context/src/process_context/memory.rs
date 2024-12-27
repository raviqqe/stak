use crate::ProcessContext;

/// A in-memory process context.
#[derive(Debug)]
pub struct MemoryProcessContext<'a> {
    arguments: &'a [&'a str],
    environment_variables: &'a [(&'a str, &'a str)],
}

impl<'a> MemoryProcessContext<'a> {
    /// Creates a process context.
    pub const fn new(
        arguments: &'a [&'a str],
        environment_variables: &'a [(&'a str, &'a str)],
    ) -> Self {
        Self {
            arguments,
            environment_variables,
        }
    }
}

impl ProcessContext for MemoryProcessContext<'_> {
    fn command_line_rev(&self) -> impl IntoIterator<Item = &str> {
        self.arguments.iter().rev().copied()
    }

    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)> {
        self.environment_variables.iter().copied()
    }
}
