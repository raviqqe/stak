use core::fmt::{self, Display, Formatter};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error {
    Open,
    Read,
    Write,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::Open => write!(f, "cannot open file"),
            Error::Read => write!(f, "cannot read file"),
            Error::Write => write!(f, "cannot write file"),
        }
    }
}
