use core::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    Halt,
    Illegal,
    ReadInput,
    Vm(vm::Error),
    #[allow(clippy::enum_variant_names)]
    WriteError,
    WriteOutput,
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl Display for Error {
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

impl From<vm::Error> for Error {
    fn from(error: vm::Error) -> Error {
        Error::Vm(error)
    }
}
