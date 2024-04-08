use crate::{value::Value, Error};
use core::fmt::{self, Display, Formatter};

/// An unreachable cons. In other words, it is a "null" pointer but not `null`
/// in Scheme.
///
/// This value means:
///
/// - in car, its cons is moved already on garbage collection.
/// - in cdr, nothing.
pub const NEVER: Cons = Cons::new(u64::MAX);

const TAG_SIZE: usize = 16;
const TAG_MASK: u64 = (1 << TAG_SIZE) - 1;

/// A cons.
#[derive(Clone, Copy, Debug)]
pub struct Cons(u64);

impl Cons {
    /// Creates a cons from a memory address on heap.
    pub const fn new(index: u64) -> Self {
        Self(index << (TAG_SIZE + 1))
    }

    /// Returns a memory address on heap.
    pub const fn index(self) -> usize {
        (self.0 >> (TAG_SIZE + 1)) as usize
    }

    /// Returns a tag.
    pub const fn tag(self) -> u16 {
        ((self.0 >> 1) & TAG_MASK) as u8
    }

    /// Sets a tag.
    pub const fn set_tag(self, tag: u16) -> Self {
        Self(((self.0 >> 1) & !TAG_MASK | (tag as u64 & TAG_MASK)) << 1)
    }

    pub(crate) const fn from_raw(raw: u64) -> Self {
        Self(raw)
    }

    pub(crate) const fn to_raw(self) -> u64 {
        self.0
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
        if self == &NEVER {
            write!(formatter, "!")?;
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
