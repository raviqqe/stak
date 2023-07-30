use crate::{value::Value, Error};
use core::fmt::{self, Display, Formatter};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Number(i64);

impl Number {
    pub const fn new(number: i64) -> Self {
        Self(number)
    }

    pub const fn to_i64(self) -> i64 {
        self.0
    }

    pub const fn to_raw(self) -> u64 {
        self.0 as u64
    }
}

impl TryFrom<Value> for Number {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        #[cfg(feature = "boost")]

        #[cfg(not(feature = "boost"))]
        value.to_number().ok_or(Error::NumberExpected)
        value.to_number().ok_or(Error::NumberExpected)
    }
}

impl Display for Number {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "n{}", self.0)
    }
}
