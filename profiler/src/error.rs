use core::num::ParseIntError;

/// An error.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Error {
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

impl From<ParseIntError> for Error {
    fn from(error: ParseIntError) -> Self {
        Self::ParseInt(error)
    }
}
