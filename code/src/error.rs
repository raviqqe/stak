use alloc::string::FromUtf8Error;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    EndOfInput,
    IllegalInstruction,
    MissingOperand,
    MissingElseBranch,
    Utf8(FromUtf8Error),
}

impl From<FromUtf8Error> for Error {
    fn from(error: FromUtf8Error) -> Self {
        Self::Utf8(error)
    }
}
