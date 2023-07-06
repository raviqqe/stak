use core::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    ArgumentCount,
    ConsExpected,
    EndOfInput,
    IllegalInstruction,
    IllegalPrimitive,
    MissingOperand,
    NumberExpected,
    OutOfMemory,
    StackUnderflow,
}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::ArgumentCount => write!(formatter, "invalid argument count"),
            Self::ConsExpected => write!(formatter, "cons expected"),
            Self::EndOfInput => write!(formatter, "unexpected end of input"),
            Self::IllegalInstruction => write!(formatter, "illegal instruction"),
            Self::IllegalPrimitive => write!(formatter, "illegal primitive"),
            Self::MissingOperand => write!(formatter, "missing operand"),
            Self::NumberExpected => write!(formatter, "number expected"),
            Self::OutOfMemory => write!(formatter, "out of memory"),
            Self::StackUnderflow => write!(formatter, "stack underflow"),
        }
    }
}
