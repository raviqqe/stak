use crate::{value::Value, Error};
use core::fmt::{self, Display, Formatter};

#[cfg(not(feature = "float"))]
type NumberRepresentation = i64;

#[cfg(feature = "float")]
type NumberRepresentation = f64;

/// A number.
///
/// It represents a signed 63-bit integer.
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Number(NumberRepresentation);

impl Number {
    /// Creates a number.
    pub const fn new(number: i64) -> Self {
        Self((number << 1) | 1)
    }

    /// Converts a number to a 64-bit -integer.
    pub const fn to_i64(self) -> i64 {
        self.0 >> 1
    }

    pub(crate) const fn from_raw(raw: u64) -> Self {
        Self(raw as i64)
    }

    pub(crate) const fn to_raw(self) -> u64 {
        self.0 as u64
    }
}

impl Default for Number {
    fn default() -> Self {
        Self::new(0)
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
