use core::fmt::{self, Display, Formatter};
use stak_primitive::SmallError;

/// A compile error.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompileError {
    /// A run error.
    Run(SmallError),
    /// A user error.
    User(String),
    /// A virtual machine error.
    Vm(stak_vm::Error),
}

impl std::error::Error for CompileError {}

impl Display for CompileError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Run(error) => write!(formatter, "{error}"),
            Self::User(error) => write!(formatter, "{error}"),
            Self::Vm(error) => write!(formatter, "{error}"),
        }
    }
}

impl From<SmallError> for CompileError {
    fn from(error: SmallError) -> Self {
        Self::Run(error)
    }
}

impl From<stak_vm::Error> for CompileError {
    fn from(error: stak_vm::Error) -> Self {
        Self::Vm(error)
    }
}
