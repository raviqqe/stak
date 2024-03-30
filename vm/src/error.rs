use core::fmt::{self, Debug, Display, Formatter};

/// An error of a virtual machine.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    /// Mismatched numbers of call arguments and function parameters.
    ArgumentCount,
    /// A cons expected.
    ConsExpected,
    /// An unexpected end of bytecodes.
    BytecodeEnd,
    /// A missing integer in bytecodes.
    BytecodeIntegerMissing,
    /// A missing operand in bytecodes.
    BytecodeOperandMissing,
    /// An illegal instruction detected.
    IllegalInstruction,
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
            Self::BytecodeEnd => write!(formatter, "unexpected end of bytecodes"),
            Self::BytecodeIntegerMissing => write!(formatter, "integer missing in bytecodes"),
            Self::BytecodeOperandMissing => write!(formatter, "operand missing in bytecodes"),
            Self::ConsExpected => write!(formatter, "cons expected"),
            Self::IllegalInstruction => write!(formatter, "illegal instruction"),
            Self::NumberExpected => write!(formatter, "number expected"),
            Self::OutOfMemory => write!(formatter, "out of memory"),
            Self::ProcedureExpected => write!(formatter, "procedure expected"),
        }
    }
}
