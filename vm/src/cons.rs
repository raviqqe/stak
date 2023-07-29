use crate::{value::Value, Error};
use core::fmt::{self, Display, Formatter};

pub const FALSE: Cons = Cons::dummy(0);
pub const TRUE: Cons = Cons::dummy(1);
pub const NULL: Cons = Cons::dummy(2);
pub const MOVED: Cons = Cons::dummy(3);

const TAG_MASK: u64 = 0b1111;
const TAG_SIZE: usize = TAG_MASK.count_ones() as usize;

#[derive(Clone, Copy, Debug)]
pub struct Cons(u64);

impl Cons {
    pub const fn new(index: u64) -> Self {
        Self(index << TAG_SIZE)
    }

    pub const fn dummy(index: u64) -> Self {
        Self::new((-1i64 - index as i64) as u64)
    }

    pub const fn index(self) -> usize {
        (self.0 >> TAG_SIZE) as usize
    }

    pub const fn tag(self) -> u8 {
        (self.0 & TAG_MASK) as u8
    }

    pub const fn set_tag(self, tag: u8) -> Self {
        Self(self.0 & !TAG_MASK | tag as u64 & TAG_MASK)
    }
}

impl PartialEq for Cons {
    fn eq(&self, other: &Self) -> bool {
        self.index() == other.index()
    }
}

impl Eq for Cons {}

impl TryFrom<Value> for Cons {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        value.to_cons().ok_or(Error::ConsExpected)
    }
}

impl Display for Cons {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        if self == &FALSE {
            write!(formatter, "#f")?;
        } else if self == &TRUE {
            write!(formatter, "#t")?;
        } else if self == &NULL {
            write!(formatter, "()")?;
        } else {
            write!(formatter, "c{:x}", self.index())?;
        }

        write!(formatter, ":{}", self.tag())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tag() {
        let cons = Cons::new(42);

        assert_eq!(cons.index(), 42);
        assert_eq!(cons.tag(), 0);

        let cons = cons.set_tag(1);

        assert_eq!(cons.index(), 42);
        assert_eq!(cons.tag(), 1);

        let cons = cons.set_tag(3);

        assert_eq!(cons.index(), 42);
        assert_eq!(cons.tag(), 3);
    }

    #[test]
    fn reset_tag() {
        assert_eq!(Cons::new(42).set_tag(2).set_tag(1).tag(), 1);
    }

    #[test]
    fn set_too_large_tag() {
        let cons = Cons::new(0).set_tag(u8::MAX);

        assert_eq!(cons.index(), 0);
        assert_eq!(cons.tag(), TAG_MASK as u8);
    }
}
