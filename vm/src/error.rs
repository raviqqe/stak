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
impl<E> std::error::Error for Error<E> {}

impl<E: Display> Display for Error<E> {
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
            Self::Primitive(error) => write!(formatter, "{}", { error }),
            Self::ProcedureExpected => write!(formatter, "procedure expected"),
            Self::StackUnderflow => write!(formatter, "stack underflow"),
        }
    }
}
