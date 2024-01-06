use core::fmt::{self, Display, Formatter};
use stak_primitive::SmallError;

/// A compile error.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    /// A virtual machine error.
    Vm(SmallError),
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Vm(error) => write!(formatter, "{}", error),
        }
    }
}

impl From<SmallError> for Error {
    fn from(error: SmallError) -> Self {
        Self::Vm(error)
    }
}
