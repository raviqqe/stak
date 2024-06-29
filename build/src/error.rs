use stak_compiler::CompileError;
use tokio::io;

pub enum BuildError {
    Compile(CompileError),
    Io(io::Error),
}

impl From<CompileError> for BuildError {
    fn from(error: CompileError) -> BuildError {
        Self::Compile(error)
    }
}

impl From<io::Error> for BuildError {
    fn from(error: io::Error) -> BuildError {
        Self::Io(error)
    }
}
