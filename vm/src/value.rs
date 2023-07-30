use crate::{cons::Cons, number::Number};
use core::fmt::{self, Display, Formatter};

#[derive(Copy, Clone, Eq, Debug, PartialEq)]
pub struct Value(u64);

impl Value {
    pub const fn to_cons(self) -> Option<Cons> {
        if self.is_cons() {
            Some(Cons::new(self.0 >> 1))
        } else {
            None
        }
    }

    pub const fn to_number(self) -> Option<Number> {
        if self.is_number() {
            Some(Number::new((self.0 as i64 >> 1) as u64))
        } else {
            None
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
        if let Some(cons) = self.to_cons() {
            write!(formatter, "{}", cons)
        } else if let Some(number) = self.to_number() {
            write!(formatter, "{}", number)
        } else {
            unreachable!()
        }
    }
}
