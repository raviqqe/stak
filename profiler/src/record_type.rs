use core::fmt::{self, Display, Formatter};

/// A record type.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RecordType {
    /// A call.
    Call,
    /// A return.
    Return,
    /// A return (tail) call.
    ReturnCall,
}

impl Display for RecordType {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Call => write!(formatter, "call"),
            Self::Return => write!(formatter, "return"),
            Self::ReturnCall => write!(formatter, "return"),
        }
    }
}
