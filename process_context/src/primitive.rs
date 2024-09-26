mod primitive_set;

pub use primitive_set::ProcessContextPrimitiveSet;

/// A primitive of process context.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Primitive {
    /// A command line.
    CommandLine,
    /// Environment variables.
    EnvironmentVariables,
}

impl Primitive {
    const COMMAND_LINE: u8 = Self::CommandLine as _;
    const ENVIRONMENT_VARIABLES: u8 = Self::EnvironmentVariables as _;
}
