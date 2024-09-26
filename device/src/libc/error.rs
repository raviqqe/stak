use core::{
    error,
    fmt::{self, Display, Formatter},
};

/// An error.
#[derive(Debug)]
pub enum LibcError {
    /// A stdin error.
    Stdin,
    /// A stdout error.
    Stdout,
    /// A stderr error.
    Stderr,
}

impl error::Error for LibcError {}

impl Display for LibcError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Stdin => write!(formatter, "failed to read stdin"),
            Self::Stdout => write!(formatter, "failed to write stdout"),
            Self::Stderr => write!(formatter, "failed to write stderr"),
        }
    }
}
