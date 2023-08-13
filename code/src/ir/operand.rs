use core::fmt::{self, Display, Formatter};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Operand {
    Symbol(u64),
    Integer(u64),
}

impl Display for Operand {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Symbol(index) => write!(formatter, "s{}", index),
            Self::Integer(integer) => write!(formatter, "i{}", integer),
        }
    }
}
