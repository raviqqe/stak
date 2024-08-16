use crate::{value::Value, Error};
use cfg_if::cfg_if;
use core::{
    fmt::{self, Display, Formatter},
    ops::{Add, Div, Mul, Rem, Sub},
};

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
        cfg_if! {
            if #[cfg(feature = "float")] {
                Self(number)
            } else {
                Self(number << 1 | 1)
            }
        }
    }

    /// Converts a number to a number representation.
    pub const fn to_representation(self) -> NumberRepresentation {
        cfg_if! {
            if #[cfg(feature = "float")] {
                self.0
            } else {
                self.0 >> 1
            }
        }
    }

    /// Converts `i64` into a number.
    pub const fn from_i64(number: i64) -> Self {
        Self::new(number as _)
    }

    /// Converts a number to a 64-bit integer.
    pub const fn to_i64(self) -> i64 {
        cfg_if! {
            if #[cfg(feature = "float")] {
                self.0 as _
            } else {
                self.0 >> 1
            }
        }
    }

    pub(crate) fn from_raw(raw: u64) -> Self {
        cfg_if! {
            if #[cfg(feature = "float")] {
                Self(f64::from_bits(raw))
            } else {
                Self(raw as _)
            }
        }
    }

    pub(crate) fn to_raw(self) -> u64 {
        cfg_if! {
            if #[cfg(feature = "float")] {
                self.0.to_bits()
            } else {
                self.0 as _
            }
        }
    }
}

impl Default for Number {
    fn default() -> Self {
        Self::new(0 as _)
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self::new(self.to_representation() + other.to_representation())
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        Self::new(self.to_representation() - other.to_representation())
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        Self::new(self.to_representation() * other.to_representation())
    }
}

impl Div for Number {
    type Output = Self;

    fn div(self, other: Self) -> Self::Output {
        Self::new(self.to_representation() / other.to_representation())
    }
}

impl Rem for Number {
    type Output = Self;

    fn rem(self, other: Self) -> Self::Output {
        Self::new(self.to_representation() % other.to_representation())
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
        write!(formatter, "n{}", self.to_representation())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::format;

    #[test]
    fn default() {
        assert_eq!(Number::default(), Number::from_i64(0));
    }

    #[test]
    fn to_i64() {
        assert_eq!(Number::default().to_i64(), 0);
        assert_eq!(Number::from_i64(42).to_i64(), 42);
        assert_eq!(Number::from_i64(-1).to_i64(), -1);
    }

    #[test]
    fn format() {
        assert_eq!(format!("{}", Number::from_i64(42)), "n42");
        assert_eq!(format!("{}", Number::from_i64(-1)), "n-1");
    }

    #[test]
    fn add() {
        assert_eq!(Number::default() + Number::from_i64(1), Number::from_i64(1));
        assert_eq!(
            Number::from_i64(1) + Number::from_i64(2),
            Number::from_i64(3)
        );
    }

    #[test]
    fn subtract() {
        assert_eq!(
            Number::default() - Number::from_i64(1),
            Number::from_i64(-1)
        );
        assert_eq!(Number::from_i64(1) - Number::default(), Number::from_i64(1));
        assert_eq!(
            Number::from_i64(3) - Number::from_i64(1),
            Number::from_i64(2)
        );
    }

    #[test]
    fn multiply() {
        assert_eq!(Number::default() * Number::from_i64(1), Number::default());
        assert_eq!(
            Number::from_i64(1) * Number::from_i64(2),
            Number::from_i64(2)
        );
        assert_eq!(
            Number::from_i64(2) * Number::from_i64(3),
            Number::from_i64(6)
        );
    }

    #[test]
    fn divide() {
        assert_eq!(Number::default() / Number::from_i64(1), Number::default());
        assert_eq!(
            Number::from_i64(2) / Number::from_i64(1),
            Number::from_i64(2)
        );
        assert_eq!(
            Number::from_i64(6) / Number::from_i64(2),
            Number::from_i64(3)
        );
    }

    #[test]
    fn remainder() {
        assert_eq!(Number::default() % Number::from_i64(1), Number::default());
        assert_eq!(
            Number::from_i64(3) % Number::from_i64(2),
            Number::from_i64(1)
        );
    }
}
