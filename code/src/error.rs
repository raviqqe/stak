use alloc::string::FromUtf8Error;
use core::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    EndOfInput,
    IllegalInstruction,
    MissingClosureBody,
    MissingInteger,
    MissingOperand,
    MissingElseBranch,
    Utf8(FromUtf8Error),
}

impl From<FromUtf8Error> for Error {
    fn from(error: FromUtf8Error) -> Self {
        Self::Utf8(error)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::EndOfInput => write!(formatter, "unexpected end of input"),
            Self::IllegalInstruction => write!(formatter, "illegal instruction"),
            Self::MissingClosureBody => write!(formatter, "missing closure body"),
            Self::MissingInteger => write!(formatter, "missing integer"),
            Self::MissingOperand => write!(formatter, "missing operand"),
            Self::MissingElseBranch => write!(formatter, "missing else branch"),
            Self::Utf8(error) => write!(formatter, "{}", error),
        }
    }
}
