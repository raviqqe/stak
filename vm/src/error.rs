use core::fmt::{self, Debug, Display, Formatter};

/// An error.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    /// Mismatch between numbers of call arguments and function parameters.
    ArgumentCount,
    /// A cons expected.
    ConsExpected,
    /// An unexpected end of input.
    EndOfInput,
    /// An illegal instruction detected.
    IllegalInstruction,
    /// A missing integer.
    MissingInteger,
    /// A missing operand.
    MissingOperand,
    /// A number expected.
    NumberExpected,
    /// Out of memory.
    OutOfMemory,
    /// A procedure expected.
    ProcedureExpected,
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::ArgumentCount => write!(formatter, "invalid argument count"),
            Self::ConsExpected => write!(formatter, "cons expected"),
            Self::EndOfInput => write!(formatter, "unexpected end of input"),
            Self::IllegalInstruction => write!(formatter, "illegal instruction"),
            Self::MissingInteger => write!(formatter, "missing integer"),
            Self::MissingOperand => write!(formatter, "missing operand"),
            Self::NumberExpected => write!(formatter, "number expected"),
            Self::OutOfMemory => write!(formatter, "out of memory"),
            Self::ProcedureExpected => write!(formatter, "procedure expected"),
        }
    }
}
