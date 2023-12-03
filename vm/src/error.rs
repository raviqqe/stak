use core::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error<E> {
    ArgumentCount,
    ConsExpected,
    EndOfInput,
    IllegalInstruction,
    MissingInteger,
    MissingOperand,
    NumberExpected,
    OutOfMemory,
    Primitive(E),
    ProcedureExpected,
    StackUnderflow,
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl<E: Display> Display for Error<E> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::ArgumentCount => write!(formatter, "invalid argument count"),
            Self::ConsExpected => write!(formatter, "cons expected"),
            Self::EndOfInput => write!(formatter, "unexpected end of input"),
            Self::Halt => write!(formatter, "halt"),
            Self::IllegalInstruction => write!(formatter, "illegal instruction"),
            Self::IllegalPrimitive => write!(formatter, "illegal primitive"),
            Self::MissingInteger => write!(formatter, "missing integer"),
            Self::MissingOperand => write!(formatter, "missing operand"),
            Self::NumberExpected => write!(formatter, "number expected"),
            Self::OutOfMemory => write!(formatter, "out of memory"),
            Self::ProcedureExpected => write!(formatter, "procedure expected"),
            Self::ReadInput => write!(formatter, "failed to read input"),
            Self::StackUnderflow => write!(formatter, "stack underflow"),
            Self::WriteError => write!(formatter, "failed to write error"),
            Self::WriteOutput => write!(formatter, "failed to write output"),
        }
    }
}
