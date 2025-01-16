use core::{
    error,
    fmt::{self, Display, Formatter},
};

/// An error.
#[derive(Debug)]
pub enum DynamicError {
    /// A object downcast error.
    Downcast,
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

impl error::Error for DynamicError {}

impl Display for DynamicError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Downcast => write!(formatter, "cannot downcast object"),
            Self::ObjectIndex => write!(formatter, "invalid object index"),
            Self::Vm(error) => write!(formatter, "{error}"),
        }
    }
}
