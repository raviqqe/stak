use core::{
    error::Error,
    fmt::{self, Display, Formatter},
};
use stak_dynamic::DynamicError;
use stak_r7rs::SmallError;
use stak_vm::Exception;

/// An engine error
#[derive(Debug)]
pub enum EngineError {
    /// A dynamic primitive error.
    Dynamic(DynamicError),
    /// An R7RS-small error.
    Small(SmallError),
    /// A virtual machine error.
    Vm(stak_vm::Error),
}

impl From<DynamicError> for EngineError {
    fn from(error: DynamicError) -> Self {
        Self::Dynamic(error)
    }
}

impl From<SmallError> for EngineError {
    fn from(error: SmallError) -> Self {
        Self::Small(error)
    }
}

impl From<stak_vm::Error> for EngineError {
    fn from(error: stak_vm::Error) -> Self {
        Self::Vm(error)
    }
}

impl Exception for EngineError {
    fn is_critical(&self) -> bool {
        match self {
            Self::Dynamic(error) => error.is_critical(),
            Self::Small(error) => error.is_critical(),
            Self::Vm(error) => error.is_critical(),
        }
    }
}

impl Error for EngineError {}

impl Display for EngineError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Dynamic(error) => write!(formatter, "{error}"),
            Self::Small(error) => write!(formatter, "{error}"),
            Self::Vm(error) => write!(formatter, "{error}"),
        }
    }
}
