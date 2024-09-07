use core::{
    error::Error,
    fmt::{self, Display, Formatter},
};
use stak_compiler::CompileError;
use tokio::io;

/// A build error.
#[derive(Debug)]
pub enum BuildError {
    /// A compile error.
    Compile(CompileError),
    /// An I/O error.
    Io(io::Error),
}

impl From<CompileError> for BuildError {
    fn from(error: CompileError) -> Self {
        Self::Compile(error)
    }
}

impl From<io::Error> for BuildError {
    fn from(error: io::Error) -> Self {
        Self::Io(error)
    }
}

impl Error for BuildError {}

impl Display for BuildError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Compile(error) => write!(formatter, "{error}"),
            Self::Io(error) => write!(formatter, "{error}"),
        }
    }
}
