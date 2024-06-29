use stak_compiler::CompileError;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};
use tokio::io;

#[derive(Debug)]
pub enum BuildError {
    Compile(CompileError),
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
            Self::Compile(error) => write!(formatter, "{}", error),
            Self::Io(error) => write!(formatter, "{}", error),
        }
    }
}
