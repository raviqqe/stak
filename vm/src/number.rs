use crate::{
    Error,
    value::Value,
    value_inner::{self, NumberInner},
};
use core::{
    fmt::{self, Display, Formatter},
    ops::{Add, Mul, Sub},
};

/// A number.
///
/// It represents a signed 63-bit integer by default. If the `float` feature is
/// enabled, it represents a 64-bit floating-point number.
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
#[cfg_attr(not(feature = "float"), derive(Eq, Ord))]
pub struct Number(NumberInner);

impl Number {
    const fn new(number: NumberInner) -> Self {
        Self(value_inner::from_number(number))
    }

    const fn to_representation(self) -> NumberInner {
        value_inner::to_number(self.0)
    }

    /// Converts `i64` into a number.
    pub const fn from_i64(number: i64) -> Self {
        Self(value_inner::from_i64(number))
    }

    /// Converts a number to `i64`.
    pub const fn to_i64(self) -> i64 {
        value_inner::to_i64(self.0)
    }

    /// Converts `f64` to a number.
    pub const fn from_f64(number: f64) -> Self {
        Self(value_inner::from_f64(number))
    }

    /// Converts a number to `f64`.
    pub const fn to_f64(self) -> f64 {
        value_inner::to_f64(self.0)
    }

    pub(crate) const fn from_raw(raw: u64) -> Self {
        Self(value_inner::from_raw(raw))
    }

    pub(crate) const fn to_raw(self) -> u64 {
        value_inner::to_raw(self.0)
    }

    /// Calculates the power.
    #[allow(clippy::missing_const_for_fn)]
    pub fn power(self, number: Self) -> Self {
        Self::new(value_inner::power(
            self.to_representation(),
            number.to_representation(),
        ))
    }

    /// Divides a number by another.
    ///
    /// It returns an error on division by zero.
    pub fn divide(self, other: Self) -> Result<Self, Error> {
        value_inner::checked_divide(self.to_representation(), other.to_representation())
            .map(Self::new)
            .ok_or(Error::DivisionByZero)
    }

    /// Calculates a remainder of a number divided by another.
    ///
    /// It returns an error on division by zero.
    pub fn remainder(self, other: Self) -> Result<Self, Error> {
        value_inner::checked_remainder(self.to_representation(), other.to_representation())
            .map(Self::new)
            .ok_or(Error::DivisionByZero)
    }
}

impl Default for Number {
    fn default() -> Self {
        Self::new(Default::default())
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
    fn integer() {
        assert_eq!(Number::default().to_i64(), 0);
        assert_eq!(Number::from_i64(42).to_i64(), 42);
        assert_eq!(Number::from_i64(-1).to_i64(), -1);
    }

    #[test]
    fn float() {
        assert_eq!(Number::default().to_f64(), 0.0);
        assert_eq!(Number::from_f64(1.0).to_f64(), 1.0);
        assert_eq!(Number::from_f64(42.0).to_f64(), 42.0);
        assert_eq!(Number::from_f64(-1.0).to_f64(), -1.0);
        assert_eq!(Number::from_f64(-42.0).to_f64(), -42.0);
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
        assert_eq!(
            Number::default().divide(Number::from_i64(1)),
            Ok(Number::default())
        );
        assert_eq!(
            Number::from_i64(2).divide(Number::from_i64(1)),
            Ok(Number::from_i64(2))
        );
        assert_eq!(
            Number::from_i64(6).divide(Number::from_i64(2)),
            Ok(Number::from_i64(3))
        );
    }

    #[test]
    fn remainder() {
        assert_eq!(
            Number::default().remainder(Number::from_i64(1)),
            Ok(Number::default())
        );
        assert_eq!(
            Number::from_i64(3).remainder(Number::from_i64(2)),
            Ok(Number::from_i64(1))
        );
    }

    #[cfg(not(feature = "float"))]
    #[test]
    fn divide_by_zero() {
        assert_eq!(
            Number::from_i64(1).divide(Number::from_i64(0)),
            Err(Error::DivisionByZero)
        );
    }

    #[cfg(not(feature = "float"))]
    #[test]
    fn remainder_by_zero() {
        assert_eq!(
            Number::from_i64(1).remainder(Number::from_i64(0)),
            Err(Error::DivisionByZero)
        );
    }
}
