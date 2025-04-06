use crate::exception::Exception;
use core::{
    error,
    fmt::{self, Debug, Display, Formatter},
};

/// An error of a virtual machine.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    /// Mismatched numbers of call arguments and procedure parameters.
    ArgumentCount,
    /// A cons expected.
    ConsExpected,
    /// An unexpected end of bytecodes.
    BytecodeEnd,
    /// A format error.
    Format(fmt::Error),
    /// An illegal instruction detected.
    IllegalInstruction,
    /// An illegal primitive detected.
    IllegalPrimitive,
    /// A number expected.
    NumberExpected,
    /// Out of memory.
    OutOfMemory,
    /// A procedure expected.
    ProcedureExpected,
}

impl Exception for Error {
    fn is_critical(&self) -> bool {
        match self {
            Self::ArgumentCount
            | Self::BytecodeEnd
            | Self::IllegalPrimitive
            | Self::NumberExpected
            | Self::ConsExpected
            | Self::ProcedureExpected => false,
            Self::Format(_) | Self::IllegalInstruction | Self::OutOfMemory => true,
        }
    }
}

impl error::Error for Error {}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::ArgumentCount => write!(formatter, "invalid argument count"),
            Self::BytecodeEnd => write!(formatter, "unexpected end of bytecodes"),
            Self::ConsExpected => write!(formatter, "cons expected"),
            Self::Format(error) => write!(formatter, "{error}"),
            Self::IllegalInstruction => write!(formatter, "illegal instruction"),
            Self::IllegalPrimitive => write!(formatter, "illegal primitive"),
            Self::NumberExpected => write!(formatter, "number expected"),
            Self::OutOfMemory => write!(formatter, "out of memory"),
            Self::ProcedureExpected => write!(formatter, "procedure expected"),
        }
    }
}

impl From<fmt::Error> for Error {
    fn from(error: fmt::Error) -> Self {
        Self::Format(error)
    }
}
