use core::fmt::{self, Display, Formatter};

/// An error.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    /// A halt of a virtual machine.
    Halt,
    /// An illegal primitive.
    Illegal,
    /// A failure to read from standard input.
    ReadInput,
    /// A virtual machine error.
    Vm(vm::Error),
    /// A failure to write to standard error.
    #[allow(clippy::enum_variant_names)]
    WriteError,
    /// A failure to write to standard output.
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
    fn from(error: vm::Error) -> Self {
        Self::Vm(error)
    }
}
