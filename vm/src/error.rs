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
    /// A missing node in bytecodes.
    BytecodeNodeMissing,
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

impl error::Error for Error {}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::ArgumentCount => write!(formatter, "invalid argument count"),
            Self::BytecodeEnd => write!(formatter, "unexpected end of bytecodes"),
            Self::BytecodeNodeMissing => write!(formatter, "node missing in bytecodes"),
            Self::ConsExpected => write!(formatter, "cons expected"),
            Self::IllegalInstruction => write!(formatter, "illegal instruction"),
            Self::IllegalPrimitive => write!(formatter, "illegal primitive"),
            Self::NumberExpected => write!(formatter, "number expected"),
            Self::OutOfMemory => write!(formatter, "out of memory"),
            Self::ProcedureExpected => write!(formatter, "procedure expected"),
        }
    }
}
