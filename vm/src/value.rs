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
            TypedValue::Cons(self.as_cons())
        } else {
            TypedValue::Number(self.as_number())
        }
    }

    pub const fn as_cons(self) -> Cons {
        Cons::from_raw(self.to_payload())
    }

    pub const fn as_number(self) -> Number {
        Number::new(self.to_payload() as i64)
    }

    pub const fn is_cons(&self) -> bool {
        self.0 & 0b1 == 0
    }

    pub const fn is_number(&self) -> bool {
        !self.is_cons()
    }

    const fn to_payload(self) -> u64 {
        ((self.0 as i64) >> 1) as u64
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cons::{FALSE, MOVED, NULL, TRUE};

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
    fn convert_false() {
        assert_eq!(Value::from(FALSE).to_cons().unwrap(), FALSE);
    }

    #[test]
    fn convert_true() {
        assert_eq!(Value::from(TRUE).to_cons().unwrap(), TRUE);
    }

    #[test]
    fn convert_null() {
        assert_eq!(Value::from(NULL).to_cons().unwrap(), NULL);
    }

    #[test]
    fn convert_moved() {
        assert_eq!(Value::from(MOVED).to_cons().unwrap(), MOVED);
    }
}
