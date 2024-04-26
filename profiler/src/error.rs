use core::num::ParseIntError;
use std::io;

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
