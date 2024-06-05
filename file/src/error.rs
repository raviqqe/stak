use core::fmt::{self, Display, Formatter};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error {
    Open,
    Close,
    Read,
    Write,
}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Open => write!(formatter, "cannot open file"),
            Self::Close => write!(formatter, "cannot close file"),
            Self::Read => write!(formatter, "cannot read file"),
            Self::Write => write!(formatter, "cannot write file"),
        }
    }
}
