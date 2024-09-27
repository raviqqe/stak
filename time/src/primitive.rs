mod primitive_set;

pub use primitive_set::TimePrimitiveSet;

/// A primitive of time.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Primitive {
    /// A command line.
    CommandLine,
    /// Environment variables.
    EnvironmentVariables,
}

impl Primitive {
    const COMMAND_LINE: usize = Self::CommandLine as _;
    const ENVIRONMENT_VARIABLES: usize = Self::EnvironmentVariables as _;
}
