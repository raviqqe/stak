use crate::error::Error;
use core::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};

/// A record type.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ProcedureRecordType {
    /// A call.
    Call,
    /// A return.
    Return,
    /// A return (tail) call.
    ReturnCall,
}

impl Display for ProcedureRecordType {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Call => write!(formatter, "call"),
            Self::Return => write!(formatter, "return"),
            Self::ReturnCall => write!(formatter, "return_call"),
        }
    }
}

impl FromStr for ProcedureRecordType {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(ProcedureRecordType::Call.to_string(), "call");
        assert_eq!(ProcedureRecordType::Return.to_string(), "return");
        assert_eq!(ProcedureRecordType::ReturnCall.to_string(), "return_call");
    }

    #[test]
    fn test_from_str() {
        assert_eq!("call".parse(), Ok(ProcedureRecordType::Call));
        assert_eq!("return".parse(), Ok(ProcedureRecordType::Return));
        assert_eq!("return_call".parse(), Ok(ProcedureRecordType::ReturnCall));
        assert_eq!(
            "unknown".parse::<ProcedureRecordType>(),
            Err(Error::UnknownRecordType)
        );
    }
}
