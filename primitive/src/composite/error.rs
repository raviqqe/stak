use core::fmt::{self, Display, Formatter};

/// A composite error.
#[derive(Debug)]
pub enum CompositeError<E, F> {
    Primary(E),
    Secondary(F),
}

/// An error that can be caused by an illegal primitive.
pub trait IllegalPrimitiveError {
    fn is_illegal_primitive(&self) -> bool;
}

impl<E: From<stak_vm::Error>, F> From<stak_vm::Error> for CompositeError<E, F> {
    fn from(error: stak_vm::Error) -> Self {
        Self::Primary(error.into())
    }
}

impl<E: Display, F: Display> Display for CompositeError<E, F> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Primary(error) => write!(formatter, "{error}"),
            Self::Secondary(error) => write!(formatter, "{error}"),
        }
    }
}
