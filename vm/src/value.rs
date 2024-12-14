use crate::{
    cons::{Cons, Tag},
    number::Number,
    Error,
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
    #[inline]
    pub const fn to_cons(self) -> Option<Cons> {
        if let TypedValue::Cons(cons) = self.to_typed() {
            Some(cons)
        } else {
            None
        }
    }

    /// Converts a value to a number.
    #[inline]
    pub const fn to_number(self) -> Option<Number> {
        if let TypedValue::Number(number) = self.to_typed() {
            Some(number)
        } else {
            None
        }
    }

    /// Converts a value to a typed value.
    #[inline]
    pub fn to_typed(self) -> TypedValue {
        Ok(if self.is_cons() {
            TypedValue::Cons(self.raw())
        } else {
            TypedValue::Number(self.assume_number())
        })
    }

    /// Converts a value to a number assuming its type.
    #[inline]
    pub const fn assume_number(self) -> Number {
        debug_assert!(self.is_number());

        Number::from_raw(self.0)
    }

    /// Checks if it is a cons.
    #[inline]
    pub const fn is_cons(&self) -> bool {
        feature!(if ("float") {
            nonbox::f64::u64::unbox_unsigned(self.0).is_some()
        } else {
            self.0 & 1 == 0
        })
    }

    /// Checks if it is a number.
    #[inline]
    pub const fn is_number(&self) -> bool {
        !self.is_cons()
    }

    /// Returns a tag.
    #[inline]
    pub const fn tag(self) -> Tag {
        if let Some(cons) = self.to_cons() {
            cons.tag()
        } else {
            0
        }
    }

    /// Sets a tag.
    #[inline]
    pub const fn set_tag(self, tag: Tag) -> Self {
        if let Some(cons) = self.to_cons() {
            Self::from_cons(cons.set_tag(tag))
        } else {
            self
        }
    }

    #[inline]
    pub(crate) const fn raw_eq(self, value: Self) -> bool {
        self.0 == value.0
    }

    const fn from_cons(cons: Cons) -> Self {
        Self(cons.to_raw())
    }
}

impl Default for Value {
    #[inline]
    fn default() -> Self {
        Number::default().into()
    }
}

impl PartialEq for Value {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.is_cons() && self.to_cons() == other.to_cons()
            || self.is_number() && self.to_number() == other.to_number()
    }
}

impl Eq for Value {}

impl From<Cons> for Value {
    #[inline]
    fn from(cons: Cons) -> Self {
        Self(cons.to_raw())
    }
}

impl From<Number> for Value {
    #[inline]
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
    use crate::{cons::NEVER, Type};

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
        assert_eq!(Value::from(NEVER).to_cons().unwrap(), NEVER);
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
