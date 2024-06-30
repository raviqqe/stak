use core::fmt::{self, Debug, Display, Formatter};

/// A composite error.
#[derive(Debug)]
pub enum CompositeError<E, F> {
    First(E),
    Second(F),
}

/// An error that can be caused by an illegal primitive.
pub trait IllegalPrimitiveError {
    fn is_illegal_primitive(&self) -> bool;
}

impl<E: Debug + Display, F: Debug + Display> From<stak_vm::Error> for CompositeError<E, F> {
    fn from(error: stak_vm::Error) -> Self {
        Self::First(error.into())
    }
}

impl<E: Display, F: Display> Display for CompositeError<E, F> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::First(error) => write!(f, "{error}"),
            Self::Second(error) => write!(f, "{error}"),
        }
    }
}
