use core::{
    error,
    fmt::{self, Display, Formatter},
};

/// An error.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FileError {
    /// An open failure.
    Open,
    /// A close failure.
    Close,
    /// An invalid file descriptor.
    InvalidFileDescriptor,
    /// A read failure.
    Read,
    /// A write failure.
    Write,
    /// A deletion failure.
    Delete,
    /// A existence check failure.
    Exists,
    /// A path decode failure.
    PathDecode,
}

impl error::Error for FileError {}

impl Display for FileError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Open => write!(formatter, "cannot open file"),
            Self::Close => write!(formatter, "cannot close file"),
            Self::InvalidFileDescriptor => write!(formatter, "invalid file descriptor"),
            Self::Read => write!(formatter, "cannot read file"),
            Self::Write => write!(formatter, "cannot write file"),
            Self::Delete => write!(formatter, "cannot delete file"),
            Self::Exists => write!(formatter, "cannot check file existence"),
            Self::PathDecode => write!(formatter, "cannot decode path"),
        }
    }
}
