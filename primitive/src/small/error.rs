use core::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SmallPrimitiveError {
    Halt,
    Illegal,
    ReadInput,
    WriteError,
    WriteOutput,
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl Display for SmallPrimitiveError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Halt => write!(formatter, "halt"),
            Self::Illegal => write!(formatter, "illegal primitive"),
            Self::ReadInput => write!(formatter, "failed to read input"),
            Self::WriteError => write!(formatter, "failed to write error"),
            Self::WriteOutput => write!(formatter, "failed to write output"),
        }
    }
}

impl Into<vm::Error> for SmallPrimitiveError {
    fn into(self) -> vm::Error {
        vm::Error::Primitive(self)
    }
}
