use crate::{Error, value::Value};
use cfg_elif::{expr::feature, item};
use core::{
    fmt::{self, Display, Formatter},
    ops::{Add, Div, Mul, Rem, Sub},
};

item::feature!(if ("float62") {
    /// A number representation.
    pub type NumberRepresentation = nonbox::f62::Float62;
} else if ("float") {
    /// A number representation.
    pub type NumberRepresentation = f64;
} else {
    /// A number representation.
    pub type NumberRepresentation = i64;
});

/// A number.
///
/// It represents a signed 63-bit integer by default. If the `float` feature is
/// enabled, it represents a 64-bit floating-point number.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
#[cfg_attr(not(feature = "float"), derive(Eq, Ord))]
pub struct Number(NumberRepresentation);

impl Number {
    /// Creates a number.
    #[inline]
    pub const fn new(number: NumberRepresentation) -> Self {
        Self(feature!(if ("float") { number } else { (number << 1) | 1 }))
    }

    /// Converts a number to a number representation.
    #[inline]
    pub const fn to_representation(self) -> NumberRepresentation {
        feature!(if ("float") { self.0 } else { self.0 >> 1 })
    }

    /// Converts `i64` into a number.
    #[inline]
    pub const fn from_i64(number: i64) -> Self {
        Self::new(feature!(if ("float62") {
            nonbox::f62::Float62::from_integer(number)
        } else {
            number as _
        }))
    }

    /// Converts a number to `i64`.
    #[inline]
    pub const fn to_i64(self) -> i64 {
        feature!(if ("float62") {
            self.0.to_float_unchecked()
        } else {
            self.to_representation() as _
        })
    }

    /// Converts `f64` to a number.
    #[inline]
    pub const fn from_f64(number: f64) -> Self {
        Self::new(feature!(if ("float62") {
            nonbox::f62::Float62::from_float(number)
        } else {
            number as _
        }))
    }

    /// Converts a number to `f64`.
    #[inline]
    pub const fn to_f64(self) -> f64 {
        feature!(if ("float62") {
            self.0.to_float_unchecked()
        } else {
            self.to_representation() as _
        })
    }

    #[inline]
    pub(crate) const fn from_raw(raw: u64) -> Self {
        Self(feature!(if ("float62") {
            nonbox::f62::Float62::from_bits(raw)
        } else if ("float") {
            f64::from_bits(raw)
        } else {
            raw as _
        }))
    }

    #[inline]
    pub(crate) const fn to_raw(self) -> u64 {
        feature!(if ("float62") {
            self.0.to_bits()
        } else if ("float") {
            self.0.to_bits()
        } else {
            self.0 as _
        })
    }
}

impl Default for Number {
    #[inline]
    fn default() -> Self {
        Self::new(NumberRepresentation::default())
    }
}

impl Add for Number {
    type Output = Self;

    #[inline]
    fn add(self, other: Self) -> Self::Output {
        Self::new(self.to_representation() + other.to_representation())
    }
}

impl Sub for Number {
    type Output = Self;

    #[inline]
    fn sub(self, other: Self) -> Self::Output {
        Self::new(self.to_representation() - other.to_representation())
    }
}

impl Mul for Number {
    type Output = Self;

    #[inline]
    fn mul(self, other: Self) -> Self::Output {
        Self::new(self.to_representation() * other.to_representation())
    }
}

impl Div for Number {
    type Output = Self;

    #[inline]
    fn div(self, other: Self) -> Self::Output {
        Self::new(self.to_representation() / other.to_representation())
    }
}

impl Rem for Number {
    type Output = Self;

    #[inline]
    fn rem(self, other: Self) -> Self::Output {
        Self::new(self.to_representation() % other.to_representation())
    }
}

impl TryFrom<Value> for Number {
    type Error = Error;

    #[inline]
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
