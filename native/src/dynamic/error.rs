use any_fn::AnyFnError;
use core::{
    error::Error,
    fmt::{self, Display, Formatter},
};

/// An error.
#[derive(Debug)]
pub enum DynamicError {
    /// An `AnyFn` error.
    AnyFn(AnyFnError),
    /// A object index error.
    ObjectIndex,
    /// A virtual machine error.
    Vm(stak_vm::Error),
}

impl From<stak_vm::Error> for DynamicError {
    fn from(error: stak_vm::Error) -> Self {
        Self::Vm(error)
    }
}

impl Error for DynamicError {}

impl Display for DynamicError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::AnyFn(error) => write!(formatter, "{error}"),
            Self::ObjectIndex => write!(formatter, "invalid object index"),
            Self::Vm(error) => write!(formatter, "{error}"),
        }
    }
}
