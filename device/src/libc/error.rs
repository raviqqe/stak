use core::fmt::{self, Debug, Formatter};

pub enum Error {
    Stdout,
    Stderr,
}

impl Debug for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Stdout => write!(formatter, "failed to write stdout"),
            Self::Stderr => write!(formatter, "failed to write stderr"),
        }
    }
}
