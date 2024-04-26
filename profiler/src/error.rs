use core::{
    fmt::{self, Display, Formatter},
    num::ParseIntError,
};
use std::{error, io};

/// An error.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Error {
    /// I/O failure.
    Io(String),
    /// A missing call record.
    MissingCallRecord,
    /// A missing record type.
    MissingRecordType,
    /// A missing stack.
    MissingStack,
    /// A missing time.
    MissingTime,
    /// Integer parse failure.
    ParseInt(ParseIntError),
    /// An unknown record type.
    UnknownRecordType,
}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Io(error) => write!(formatter, "{error}"),
            Self::MissingCallRecord => write!(formatter, "missing call record"),
            Self::MissingRecordType => write!(formatter, "missing record type"),
            Self::MissingStack => write!(formatter, "missing stack"),
            Self::MissingTime => write!(formatter, "missing time"),
            Self::ParseInt(error) => write!(formatter, "{error}"),
            Self::UnknownRecordType => write!(formatter, "unknown record type"),
        }
    }
}

impl error::Error for Error {}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Self::Io(error.to_string())
    }
}

impl From<ParseIntError> for Error {
    fn from(error: ParseIntError) -> Self {
        Self::ParseInt(error)
    }
}
