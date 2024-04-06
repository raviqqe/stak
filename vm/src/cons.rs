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

const TAG_SIZE: usize = 4;
const TAG_MASK: u64 = (1 << TAG_SIZE) - 1;
const META_SIZE: usize = 12;
const META_MASK: u64 = (1 << META_SIZE) - 1;
const META_OFFSET: usize = TAG_SIZE + 1;
const INDEX_OFFSET: usize = META_OFFSET + META_SIZE;

/// A cons.
#[derive(Clone, Copy, Debug)]
pub struct Cons(u64);

impl Cons {
    /// Creates a cons from a memory address on heap.
    pub const fn new(index: u64) -> Self {
        Self(index << INDEX_OFFSET)
    }

    /// Returns a memory address on heap.
    pub const fn index(self) -> usize {
        (self.0 >> INDEX_OFFSET) as usize
    }

    /// Returns a tag.
    pub const fn tag(self) -> u8 {
        ((self.0 >> 1) & TAG_MASK) as u8
    }

    /// Sets a tag.
    pub const fn set_tag(self, tag: u8) -> Self {
        Self(((self.0 >> 1) & !TAG_MASK | (tag as u64 & TAG_MASK)) << 1)
    }

    /// Returns metadata.
    pub const fn meta(self) -> u16 {
        ((self.0 >> META_OFFSET) & META_MASK) as u16
    }

    /// Sets metadata.
    pub const fn set_meta(self, meta: u16) -> Self {
        Self(
            self.0 & (u64::MAX - (META_MASK << META_OFFSET))
                | ((meta as u64 & META_MASK) << META_OFFSET),
        )
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

    mod tag {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn get_and_set() {
            const META: u16 = 13;

            let cons = Cons::new(42).set_meta(META);

            assert_eq!(cons.index(), 42);
            assert_eq!(cons.tag(), 0);
            assert_eq!(cons.meta(), META);

            let cons = cons.set_tag(1);

            assert_eq!(cons.index(), 42);
            assert_eq!(cons.tag(), 1);
            assert_eq!(cons.meta(), META);

            let cons = cons.set_tag(3);

            assert_eq!(cons.index(), 42);
            assert_eq!(cons.tag(), 3);
            assert_eq!(cons.meta(), META);
        }

        #[test]
        fn reset() {
            assert_eq!(Cons::new(42).set_tag(2).set_tag(1).tag(), 1);
        }

        #[test]
        fn set_too_large() {
            let cons = Cons::new(0).set_tag(u8::MAX);

            assert_eq!(cons.index(), 0);
            assert_eq!(cons.tag(), TAG_MASK as u8);
        }
    }

    mod meta {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn get_and_set() {
            const TAG: u8 = 5;

            let cons = Cons::new(42).set_tag(TAG);

            assert_eq!(cons.index(), 42);
            assert_eq!(cons.tag(), TAG);
            assert_eq!(cons.meta(), 0);

            let cons = cons.set_meta(1);

            assert_eq!(cons.index(), 42);
            assert_eq!(cons.tag(), TAG);
            assert_eq!(cons.meta(), 1);

            let cons = cons.set_meta(3);

            assert_eq!(cons.index(), 42);
            assert_eq!(cons.tag(), TAG);
            assert_eq!(cons.meta(), 3);
        }

        #[test]
        fn reset() {
            assert_eq!(Cons::new(42).set_meta(2).set_meta(1).meta(), 1);
        }

        #[test]
        fn set_too_large() {
            let cons = Cons::new(0).set_meta(u16::MAX);

            assert_eq!(cons.index(), 0);
            assert_eq!(cons.meta(), META_MASK as u16);
        }
    }
}
