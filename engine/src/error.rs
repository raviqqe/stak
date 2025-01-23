use core::{
    error::Error,
    fmt::{self, Display, Formatter},
};
use stak_native::dynamic::DynamicError;
use stak_r7rs::SmallError;

/// A script error
#[derive(Debug)]
pub enum ScriptError {
    /// An R7RS-small error.
    Small(SmallError),
    /// A dynamic primitive error.
    Dynamic(DynamicError),
    /// A virtual machine error.
    Vm(stak_vm::Error),
}

impl From<stak_vm::Error> for ScriptError {
    fn from(error: stak_vm::Error) -> Self {
        Self::Vm(error)
    }
}

impl From<SmallError> for ScriptError {
    fn from(error: SmallError) -> Self {
        Self::Small(error)
    }
}

impl From<DynamicError> for ScriptError {
    fn from(error: DynamicError) -> Self {
        Self::Dynamic(error)
    }
}

impl Error for ScriptError {}

impl Display for ScriptError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Small(error) => write!(formatter, "{error}"),
            Self::Dynamic(error) => write!(formatter, "{error}"),
            Self::Vm(error) => write!(formatter, "{error}"),
        }
    }
}
