use core::fmt::{self, Display, Formatter};

use crate::{value::Value, Error};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Number(u64);

impl Number {
    pub const fn new(number: u64) -> Self {
        Self(number)
    }

    pub const fn to_u64(self) -> u64 {
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
        write!(formatter, "n{}", self.0)
    }
}
