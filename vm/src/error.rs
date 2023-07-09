use core::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    ArgumentCount,
    CellIndexOutOfRange,
    ConsExpected,
    EndOfInput,
    HeapSize,
    IllegalInstruction,
    IllegalPrimitive,
    MissingOperand,
    NumberExpected,
    OutOfMemory,
    ProcedureExpected,
    ReadInput,
    StackUnderflow,
    WriteOutput,
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::ArgumentCount => write!(formatter, "invalid argument count"),
            Self::CellIndexOutOfRange => write!(formatter, "cell index out of range"),
            Self::ConsExpected => write!(formatter, "cons expected"),
            Self::EndOfInput => write!(formatter, "unexpected end of input"),
            Self::HeapSize => write!(formatter, "too small heap size"),
            Self::IllegalInstruction => write!(formatter, "illegal instruction"),
            Self::IllegalPrimitive => write!(formatter, "illegal primitive"),
            Self::MissingOperand => write!(formatter, "missing operand"),
            Self::NumberExpected => write!(formatter, "number expected"),
            Self::OutOfMemory => write!(formatter, "out of memory"),
            Self::ProcedureExpected => write!(formatter, "procedure expected"),
            Self::ReadInput => write!(formatter, "failed to read input"),
            Self::StackUnderflow => write!(formatter, "stack underflow"),
            Self::WriteOutput => write!(formatter, "failed to write output"),
        }
    }
}
