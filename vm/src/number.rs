use crate::{value::Value, Error};
use core::fmt::{self, Display, Formatter};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Number(u64);

impl Number {
    pub const fn new(number: i64) -> Self {
        Self(((number << 1) | 1) as u64)
    }

    pub const fn to_i64(self) -> i64 {
        self.0 as i64 >> 1
    }

    pub const fn from_raw(raw: u64) -> Self {
        Self(raw)
    }

    pub const fn to_raw(self) -> u64 {
        self.0
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
        write!(formatter, "n{}", self.0 >> 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_i64() {
        assert_eq!(Number::new(0).to_i64(), 0);
        assert_eq!(Number::new(42).to_i64(), 42);
        assert_eq!(Number::new(-1).to_i64(), -1);
    }
}
