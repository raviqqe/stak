use core::fmt::{self, Debug, Display, Formatter};

/// An error of a virtual machine.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    /// Mismatched numbers of call arguments and function parameters.
    ArgumentCount,
    /// A cons expected.
    ConsExpected,
    /// An unexpected end of bytecodes.
    EndOfBytecodes,
    /// An illegal instruction detected.
    IllegalInstruction,
    /// A missing integer in bytecodes.
    MissingInteger,
    /// A missing operand in bytecodes.
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
            Self::EndOfBytecodes => write!(formatter, "unexpected end of bytecodes"),
            Self::IllegalInstruction => write!(formatter, "illegal instruction"),
            Self::MissingInteger => write!(formatter, "integer missing in bytecodes"),
            Self::MissingOperand => write!(formatter, "operand missing in bytecodes"),
            Self::NumberExpected => write!(formatter, "number expected"),
            Self::OutOfMemory => write!(formatter, "out of memory"),
            Self::ProcedureExpected => write!(formatter, "procedure expected"),
        }
    }
}
