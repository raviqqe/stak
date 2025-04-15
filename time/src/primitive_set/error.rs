use core::{
    error,
    fmt::{self, Debug, Display, Formatter},
};
use stak_vm::Exception;

/// An error of primitives.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PrimitiveError {
    /// A current jiffy error.
    CurrentJiffy,
    /// A virtual machine error.
    Vm(stak_vm::Error),
}

impl Exception for PrimitiveError {
    fn is_critical(&self) -> bool {
        match self {
            Self::CurrentJiffy => false,
            Self::Vm(error) => error.is_critical(),
        }
    }
}

impl error::Error for PrimitiveError {}

impl Display for PrimitiveError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::CurrentJiffy => write!(formatter, "failed to get current jiffy"),
            Self::Vm(error) => write!(formatter, "{error}"),
        }
    }
}

impl From<stak_vm::Error> for PrimitiveError {
    fn from(error: stak_vm::Error) -> Self {
        Self::Vm(error)
    }
}
