use core::{
    error,
    fmt::{self, Display, Formatter},
};

/// An error of primitives.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    /// A device error.
    Device(stak_device::PrimitiveError),
    /// A file error.
    File(stak_file::PrimitiveError),
    /// A halt of a virtual machine.
    Halt,
    /// A virtual machine error.
    Vm(stak_vm::Error),
}

impl error::Error for Error {}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Device(error) => write!(formatter, "{error}"),
            Self::File(error) => write!(formatter, "{error}"),
            Self::Halt => write!(formatter, "halt"),
            Self::Vm(error) => write!(formatter, "{error}"),
        }
    }
}

impl From<stak_vm::Error> for Error {
    fn from(error: stak_vm::Error) -> Self {
        Self::Vm(error)
    }
}

impl From<stak_device::PrimitiveError> for Error {
    fn from(error: stak_device::PrimitiveError) -> Self {
        Self::Device(error)
    }
}

impl From<stak_file::PrimitiveError> for Error {
    fn from(error: stak_file::PrimitiveError) -> Self {
        Self::File(error)
    }
}
