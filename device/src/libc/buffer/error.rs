use core::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BufferError {
    Read,
    Write,
}

impl Display for BufferError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Read => write!(formatter, "failed to read buffer"),
            Self::Write => write!(formatter, "failed to write buffer"),
        }
    }
}
