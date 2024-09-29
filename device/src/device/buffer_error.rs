use core::{
    error,
    fmt::{self, Display, Formatter},
};

/// A buffer error.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BufferError {
    /// A read error.
    Read,
    /// A write error.
    Write,
}

impl error::Error for BufferError {}

impl Display for BufferError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Read => write!(formatter, "failed to read buffer"),
            Self::Write => write!(formatter, "failed to write buffer"),
        }
    }
}
