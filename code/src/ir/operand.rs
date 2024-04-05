use core::fmt::{self, Display, Formatter};

/// An instruction operand.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Operand {
    /// A symbol operand.
    Symbol(u64),
    /// An integer operand.
    Integer(u64),
}

impl Display for Operand {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Symbol(index) => write!(formatter, "s{index}"),
            Self::Integer(integer) => write!(formatter, "i{integer}"),
        }
    }
}
