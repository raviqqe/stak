use core::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SmallPrimitiveError {
    Halt,
    Illegal,
    ReadInput,
    Vm(vm::Error),
    WriteError,
    WriteOutput,
}

#[cfg(feature = "std")]
impl std::error::Error for SmallPrimitiveError {}

impl Display for SmallPrimitiveError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Halt => write!(formatter, "halt"),
            Self::Illegal => write!(formatter, "illegal primitive"),
            Self::ReadInput => write!(formatter, "failed to read input"),
            Self::Vm(error) => write!(formatter, "{}", error),
            Self::WriteError => write!(formatter, "failed to write error"),
            Self::WriteOutput => write!(formatter, "failed to write output"),
        }
    }
}

impl From<vm::Error> for SmallPrimitiveError {
    fn from(error: vm::Error) -> SmallPrimitiveError {
        SmallPrimitiveError::Vm(error)
    }
}
