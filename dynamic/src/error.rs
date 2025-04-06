use any_fn::AnyFnError;
use core::{
    error::Error,
    fmt::{self, Display, Formatter},
};
use stak_vm::Exception;

/// An error.
#[derive(Debug)]
pub enum DynamicError {
    /// An `AnyFn` error.
    AnyFn(AnyFnError),
    /// A foreign value expected.
    ForeignValueExpected,
    /// A value index error.
    ValueIndex,
    /// A virtual machine error.
    Vm(stak_vm::Error),
}

impl From<AnyFnError> for DynamicError {
    fn from(error: AnyFnError) -> Self {
        Self::AnyFn(error)
    }
}

impl From<stak_vm::Error> for DynamicError {
    fn from(error: stak_vm::Error) -> Self {
        Self::Vm(error)
    }
}

impl Exception for DynamicError {
    fn is_critical(&self) -> bool {
        match self {
            Self::AnyFn(_) | Self::ForeignValueExpected | Self::ValueIndex => true,
            Self::Vm(error) => error.is_critical(),
        }
    }
}

impl Error for DynamicError {}

impl Display for DynamicError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::AnyFn(error) => write!(formatter, "{error}"),
            Self::ForeignValueExpected => write!(formatter, "foreign value expected"),
            Self::ValueIndex => write!(formatter, "invalid value index"),
            Self::Vm(error) => write!(formatter, "{error}"),
        }
    }
}
