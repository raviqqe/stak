use crate::{cons::Cons, number::Number};
use core::fmt::{self, Display, Formatter};

#[allow(dead_code)]
#[derive(Copy, Clone, Eq, Debug, PartialEq)]
pub enum Value {
    Cons(Cons),
    Number(Number),
}

#[allow(dead_code)]
impl Value {
    pub const fn to_cons(self) -> Option<Cons> {
        if let Self::Cons(x) = self {
            Some(x)
        } else {
            None
        }
    }

    pub const fn to_number(self) -> Option<Number> {
        if let Self::Number(x) = self {
            Some(x)
        } else {
            None
        }
    }

    pub const fn is_cons(&self) -> bool {
        matches!(self, Value::Cons(_))
    }

    pub const fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }
}

impl From<Cons> for Value {
    fn from(cons: Cons) -> Self {
        Self::Cons(cons)
    }
}

impl From<Number> for Value {
    fn from(number: Number) -> Self {
        Self::Number(number)
    }
}

impl Display for Value {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Cons(cons) => write!(formatter, "{}", cons),
            Self::Number(number) => write!(formatter, "{}", number),
        }
    }
}
