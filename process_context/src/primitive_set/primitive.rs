/// A primitive of process context.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Primitive {
    /// A command line.
    CommandLine,
    /// Environment variables.
    EnvironmentVariables,
}

impl Primitive {
    pub(super) const COMMAND_LINE: usize = Self::CommandLine as _;
    pub(super) const ENVIRONMENT_VARIABLES: usize = Self::EnvironmentVariables as _;
}
