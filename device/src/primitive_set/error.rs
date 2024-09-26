use core::{
    error,
    fmt::{self, Display, Formatter},
};

/// An error of primitives.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    /// A failure to read from standard input.
    ReadInput,
    /// A virtual machine error.
    Vm(stak_vm::Error),
    /// A failure to write to standard error.
    #[allow(clippy::enum_variant_names)]
    WriteError,
    /// A failure to write to standard output.
    WriteOutput,
}

impl error::Error for Error {}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::ReadInput => write!(formatter, "failed to read input"),
            Self::Vm(error) => write!(formatter, "{}", error),
            Self::WriteError => write!(formatter, "failed to write error"),
            Self::WriteOutput => write!(formatter, "failed to write output"),
        }
    }
}

impl From<stak_vm::Error> for Error {
    fn from(error: stak_vm::Error) -> Self {
        Self::Vm(error)
    }
}
