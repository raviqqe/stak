use core::fmt::{self, Debug, Formatter};

pub enum Error {
    Stdin,
    Stdout,
    Stderr,
}

impl Debug for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Stdin => write!(formatter, "failed to read stdin"),
            Self::Stdout => write!(formatter, "failed to write stdout"),
            Self::Stderr => write!(formatter, "failed to write stderr"),
        }
    }
}
