use crate::{value::Value, Error};
use cfg_if::cfg_if;
use core::fmt::{self, Display, Formatter};

/// A tag.
pub type Tag = u16;

/// An unreachable cons. In other words, it is a "null" pointer but not `null`
/// in Scheme.
///
/// This value means:
///
/// - in car, its cons is moved already on garbage collection.
/// - in cdr, nothing.
pub fn never() -> Cons {
    Cons::new(u64::MAX)
}

const TAG_SIZE: usize = Tag::BITS as usize;
const TAG_MASK: u64 = Tag::MAX as u64;

/// A cons.
#[derive(Clone, Copy, Debug)]
pub struct Cons(u64);

impl Cons {
    /// Creates a cons from a memory address on heap.
    pub fn new(index: u64) -> Self {
        Self::r#box(index << TAG_SIZE)
    }

    /// Returns a memory address on heap.
    pub fn index(self) -> usize {
        (self.unbox() >> TAG_SIZE) as _
    }

    /// Returns a tag.
    pub fn tag(self) -> Tag {
        (self.unbox() & TAG_MASK) as _
    }

    /// Sets a tag.
    pub fn set_tag(self, tag: Tag) -> Self {
        Self::r#box(self.unbox() & !TAG_MASK | (tag as u64 & TAG_MASK))
    }

    fn r#box(value: u64) -> Self {
        cfg_if! {
            if #[cfg(feature = "float")] {
                return Self(nonbox::f64::u64::box_unsigned(value));
            } else {
                return Self(value << 1);
            }
        }
    }

    fn unbox(self) -> u64 {
        cfg_if! {
            if #[cfg(feature = "float")] {
                return nonbox::f64::u64::unbox_unsigned(self.0).unwrap();
            } else {
                return self.0 >> 1;
            }
        }
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
        if self == &never() {
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
        let cons = Cons::new(0).set_tag(Tag::MAX);

        assert_eq!(cons.index(), 0);
        assert_eq!(cons.tag(), TAG_MASK as Tag);
    }
}
