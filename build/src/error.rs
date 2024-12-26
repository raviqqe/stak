use core::{
    error::Error,
    fmt::{self, Display, Formatter},
};
use glob::{GlobError, PatternError};
use stak_compiler::CompileError;
use std::env::VarError;
use tokio::{io, task::JoinError};

/// A build error.
#[derive(Debug)]
pub enum BuildError {
    /// A compile error.
    Compile(CompileError),
    /// An environment variable error.
    EnvironmentVariable(VarError),
    /// An I/O error.
    Io(io::Error),
    /// A future join error.
    Join(JoinError),
    /// A file glob error.
    Glob(GlobError),
    /// A file glob pattern error.
    GlobPattern(PatternError),
}

impl From<CompileError> for BuildError {
    fn from(error: CompileError) -> Self {
        Self::Compile(error)
    }
}

impl From<GlobError> for BuildError {
    fn from(error: GlobError) -> Self {
        Self::Glob(error)
    }
}

impl From<io::Error> for BuildError {
    fn from(error: io::Error) -> Self {
        Self::Io(error)
    }
}

impl From<JoinError> for BuildError {
    fn from(error: JoinError) -> Self {
        Self::Join(error)
    }
}

impl From<PatternError> for BuildError {
    fn from(error: PatternError) -> Self {
        Self::GlobPattern(error)
    }
}

impl From<VarError> for BuildError {
    fn from(error: VarError) -> Self {
        Self::EnvironmentVariable(error)
    }
}

impl Error for BuildError {}

impl Display for BuildError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Compile(error) => write!(formatter, "{error}"),
            Self::EnvironmentVariable(error) => write!(formatter, "{error}"),
            Self::Glob(error) => write!(formatter, "{error}"),
            Self::GlobPattern(error) => write!(formatter, "{error}"),
            Self::Io(error) => write!(formatter, "{error}"),
            Self::Join(error) => write!(formatter, "{error}"),
        }
    }
}
