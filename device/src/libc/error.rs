use core::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum Error {
    Stdin,
    Stdout,
    Stderr,
}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Stdin => write!(formatter, "failed to read stdin"),
            Self::Stdout => write!(formatter, "failed to write stdout"),
            Self::Stderr => write!(formatter, "failed to write stderr"),
        }
    }
}
