use core::{
    error,
    fmt::{self, Display, Formatter},
};
use stak_vm::Exception;

/// An error of primitives.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    /// A device error.
    Device(stak_device::PrimitiveError),
    /// A file error.
    File(stak_file::PrimitiveError),
    /// A halt of a virtual machine.
    Halt,
    /// A time error.
    Time(stak_time::PrimitiveError),
    /// A virtual machine error.
    Vm(stak_vm::Error),
}

impl Exception for Error {
    fn is_critical(&self) -> bool {
        match self {
            Self::Device(error) => error.is_critical(),
            Self::File(error) => error.is_critical(),
            Self::Halt => true,
            Self::Time(error) => error.is_critical(),
            Self::Vm(error) => error.is_critical(),
        }
    }
}

impl error::Error for Error {}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Device(error) => write!(formatter, "{error}"),
            Self::File(error) => write!(formatter, "{error}"),
            Self::Halt => write!(formatter, "halt"),
            Self::Time(error) => write!(formatter, "{error}"),
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

impl From<stak_time::PrimitiveError> for Error {
    fn from(error: stak_time::PrimitiveError) -> Self {
        Self::Time(error)
    }
}
