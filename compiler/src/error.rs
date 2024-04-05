use core::fmt::{self, Display, Formatter};
use stak_primitive::SmallError;

/// A compile error.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompileError {
    /// A user error.
    User(String),
    /// A virtual machine error.
    Vm(SmallError),
}

impl std::error::Error for CompileError {}

impl Display for CompileError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::User(error) => write!(formatter, "{error}"),
            Self::Vm(error) => write!(formatter, "{error}"),
        }
    }
}

impl From<SmallError> for CompileError {
    fn from(error: SmallError) -> Self {
        Self::Vm(error)
    }
}
