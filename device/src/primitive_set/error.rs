use core::{
    error,
    fmt::{self, Display, Formatter},
};
use stak_vm::Exception;

/// An error of primitives.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PrimitiveError {
    /// A failure to read from standard input.
    ReadInput,
    /// A virtual machine error.
    Vm(stak_vm::Error),
    /// A failure to write to standard error.
    WriteError,
    /// A failure to write to standard output.
    WriteOutput,
}

impl Exception for PrimitiveError {
    fn is_critical(&self) -> bool {
        match self {
            Self::ReadInput | Self::WriteError | Self::WriteOutput => false,
            Self::Vm(error) => error.is_critical(),
        }
    }
}

impl error::Error for PrimitiveError {}

impl Display for PrimitiveError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::ReadInput => write!(formatter, "failed to read input"),
            Self::Vm(error) => write!(formatter, "{error}"),
            Self::WriteError => write!(formatter, "failed to write error"),
            Self::WriteOutput => write!(formatter, "failed to write output"),
        }
    }
}

impl From<stak_vm::Error> for PrimitiveError {
    fn from(error: stak_vm::Error) -> Self {
        Self::Vm(error)
    }
}
