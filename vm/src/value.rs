use crate::{
    cons::{Cons, Tag},
    number::Number,
};
use cfg_exif::feature;
use core::fmt::{self, Display, Formatter};

/// A value.
#[derive(Copy, Clone, Debug)]
pub struct Value(u64);

/// A typed value.
#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(not(feature = "float"), derive(Eq))]
pub enum TypedValue {
    Cons(Cons),
    Number(Number),
}

impl Value {
    /// Converts a value to a cons.
    pub fn to_cons(self) -> Option<Cons> {
        if let TypedValue::Cons(cons) = self.to_typed() {
            Some(cons)
        } else {
            None
        }
    }

    /// Converts a value to a number.
    pub fn to_number(self) -> Option<Number> {
        if let TypedValue::Number(number) = self.to_typed() {
            Some(number)
        } else {
            None
        }
    }

    /// Converts a value to a typed value.
    pub fn to_typed(self) -> TypedValue {
        if self.is_cons() {
            TypedValue::Cons(self.assume_cons())
        } else {
            TypedValue::Number(self.assume_number())
        }
    }

    /// Converts a value to a cons assuming its type.
    pub fn assume_cons(self) -> Cons {
        debug_assert!(self.is_cons());

        Cons::from_raw(self.0)
    }

    /// Converts a value to a number assuming its type.
    pub fn assume_number(self) -> Number {
        debug_assert!(self.is_number());

        Number::from_raw(self.0)
    }

    /// Checks if it is a cons.
    pub fn is_cons(&self) -> bool {
        feature!(if ("float") {
            nonbox::f64::u64::unbox_unsigned(self.0).is_some()
        } else {
            self.0 & 1 == 0
        })
    }

    /// Checks if it is a number.
    pub fn is_number(&self) -> bool {
        !self.is_cons()
    }

    /// Returns a tag.
    pub fn tag(self) -> Tag {
        self.to_cons().map_or(0, Cons::tag)
    }

    /// Sets a tag.
    pub fn set_tag(self, tag: Tag) -> Self {
        self.to_cons().map_or(self, |cons| cons.set_tag(tag).into())
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
            TypedValue::Cons(cons) => write!(formatter, "{cons}"),
            TypedValue::Number(number) => write!(formatter, "{number}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{cons::never, Type};

    #[test]
    fn convert_cons() {
        let cons = Cons::new(42);

        assert_eq!(Value::from(cons).to_cons().unwrap(), cons);
    }

    #[test]
    fn convert_tagged_cons() {
        const TAG: Tag = 0b111;

        let cons = Cons::new(42).set_tag(TAG);
        let converted = Value::from(cons).to_cons().unwrap();

        assert_eq!(converted, cons);
        assert_eq!(converted.tag(), TAG);
    }

    #[test]
    fn convert_number() {
        let number = Number::from_i64(42);

        assert_eq!(Value::from(number).to_number().unwrap(), number);
    }

    #[test]
    fn convert_moved() {
        assert_eq!(Value::from(never()).to_cons().unwrap(), never());
    }

    #[test]
    fn get_tag_from_number() {
        let tag = Value::from(Number::from_i64(42)).tag();

        assert_eq!(tag, Default::default());
        assert_eq!(tag, Type::default() as Tag);
    }

    #[test]
    fn set_tag_to_number() {
        let value = Value::from(Number::from_i64(42)).set_tag(0b111);

        assert_eq!(value.tag(), Default::default());
        assert_eq!(value.to_number(), Some(Number::from_i64(42)));
    }
}
