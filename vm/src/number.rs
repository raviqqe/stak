use crate::{value::Value, Error};
use core::fmt::{self, Display, Formatter};

/// A number representation.
#[cfg(feature = "float")]
pub type NumberRepresentation = f64;

/// A number representation.
#[cfg(not(feature = "float"))]
pub type NumberRepresentation = i64;

/// A number.
///
/// It represents a signed 63-bit integer by default. If the `float` feature is
/// enabled, it represents a 64-bit floating-point number.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
#[cfg_attr(not(feature = "float"), derive(Eq, Ord))]
pub struct Number(NumberRepresentation);

impl Number {
    /// Creates a number.
    pub const fn new(number: NumberRepresentation) -> Self {
        #[cfg(feature = "float")]
        return Self(number);
        #[cfg(not(feature = "float"))]
        return Self(number << 1 | 1);
    }

    /// Converts a number to a 64-bit integer.
    pub const fn to_i64(self) -> i64 {
        #[cfg(feature = "float")]
        return self.0 as i64;
        #[cfg(not(feature = "float"))]
        return self.0 >> 1;
    }

    /// Converts a number to a 64-bit floating-point number.
    pub const fn to_f64(self) -> f64 {
        self.0 as f64
    }

    pub(crate) fn from_raw(raw: u64) -> Self {
        #[cfg(feature = "float")]
        return Self(f64::from_bits(raw));
        #[cfg(not(feature = "float"))]
        return Self(raw as NumberRepresentation);
    }

    pub(crate) const fn to_raw(self) -> u64 {
        self.0 as u64
    }
}

impl Default for Number {
    fn default() -> Self {
        Self::new(0 as NumberRepresentation)
    }
}

impl TryFrom<Value> for Number {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        value.to_number().ok_or(Error::NumberExpected)
    }
}

impl Display for Number {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "n{}", self.to_i64())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::format;

    #[test]
    fn default() {
        assert_eq!(Number::default(), Number::new(0));
    }

    #[test]
    fn to_i64() {
        assert_eq!(Number::new(0).to_i64(), 0);
        assert_eq!(Number::new(42).to_i64(), 42);
        assert_eq!(Number::new(-1).to_i64(), -1);
    }

    #[test]
    fn format() {
        assert_eq!(format!("{}", Number::new(42)), "n42");
        assert_eq!(format!("{}", Number::new(-1)), "n-1");
    }
}
