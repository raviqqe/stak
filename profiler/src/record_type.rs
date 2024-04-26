use crate::error::Error;
use core::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};

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

impl FromStr for RecordType {
    type Err = Error;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        match string {
            "call" => Ok(Self::Call),
            "return" => Ok(Self::Return),
            "return_call" => Ok(Self::ReturnCall),
            _ => Err(Error::UnknownRecordType),
        }
    }
}
