use crate::{cons::Cons, number::Number};
use core::fmt::{self, Display, Formatter};

#[derive(Copy, Clone, Eq, Debug, PartialEq)]
pub struct Value(u64);

#[derive(Copy, Clone, Eq, Debug, PartialEq)]
pub enum TypedValue {
    Cons(Cons),
    Number(Number),
}

impl Value {
    pub const fn to_cons(self) -> Option<Cons> {
        if let TypedValue::Cons(cons) = self.to_typed() {
            Some(cons)
        } else {
            None
        }
    }

    pub const fn to_number(self) -> Option<Number> {
        if let TypedValue::Number(number) = self.to_typed() {
            Some(number)
        } else {
            None
        }
    }

    pub const fn to_typed(self) -> TypedValue {
        if self.is_cons() {
            TypedValue::Cons(Cons::new(self.0 >> 1))
        } else {
            TypedValue::Number(Number::new(self.0 >> 1))
        }
    }

    pub const fn is_cons(&self) -> bool {
        self.0 & 0b1 == 0
    }

    pub const fn is_number(&self) -> bool {
        !self.is_cons()
    }
}

impl From<Cons> for Value {
    fn from(cons: Cons) -> Self {
        Self(cons.to_raw() << 1)
    }
}

impl From<Number> for Value {
    fn from(number: Number) -> Self {
        Self((number.to_raw() << 1) | 0b1)
    }
}

impl Display for Value {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self.to_typed() {
            TypedValue::Cons(cons) => write!(formatter, "{}", cons),
            TypedValue::Number(number) => write!(formatter, "{}", number),
        }
    }
}
