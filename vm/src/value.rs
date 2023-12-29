use crate::{cons::Cons, number::Number};
use core::fmt::{self, Display, Formatter};

#[derive(Copy, Clone, Debug)]
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
            TypedValue::Cons(self.assume_cons())
        } else {
            TypedValue::Number(self.assume_number())
        }
    }

    pub const fn assume_cons(self) -> Cons {
        debug_assert!(self.is_cons());

        Cons::from_raw(self.0)
    }

    pub const fn assume_number(self) -> Number {
        debug_assert!(self.is_number());

        Number::from_raw(self.0)
    }

    pub const fn is_cons(&self) -> bool {
        self.0 & 1 == 0
    }

    pub const fn is_number(&self) -> bool {
        !self.is_cons()
    }

    pub const fn tag(self) -> u8 {
        if let Some(cons) = self.to_cons() {
            cons.tag()
        } else {
            0
        }
    }

    pub fn set_tag(self, tag: u8) -> Self {
        if let Some(cons) = self.to_cons() {
            cons.set_tag(tag).into()
        } else {
            self
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Number::default().into()
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.is_cons() && self.to_cons() == other.to_cons()
            || self.is_number() && self.to_number() == other.to_number()
    }
}

impl Eq for Value {}

impl From<Cons> for Value {
    fn from(cons: Cons) -> Self {
        Self(cons.to_raw())
    }
}

impl From<Number> for Value {
    fn from(number: Number) -> Self {
        Self(number.to_raw())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cons::NEVER;

    #[test]
    fn convert_cons() {
        let cons = Cons::new(42);

        assert_eq!(Value::from(cons).to_cons().unwrap(), cons);
    }

    #[test]
    fn convert_tagged_cons() {
        const TAG: u8 = 0b111;

        let cons = Cons::new(42).set_tag(TAG);
        let converted = Value::from(cons).to_cons().unwrap();

        assert_eq!(converted, cons);
        assert_eq!(converted.tag(), TAG);
    }

    #[test]
    fn convert_number() {
        let number = Number::new(42);

        assert_eq!(Value::from(number).to_number().unwrap(), number);
    }

    #[test]
    fn convert_moved() {
        assert_eq!(Value::from(NEVER).to_cons().unwrap(), NEVER);
    }
}
