use stak_compiler::CompileError;
use tokio::io;

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
