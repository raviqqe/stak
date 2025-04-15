use crate::{Error, value::Value, value_inner};
use core::fmt::{self, Display, Formatter};

/// A tag.
pub type Tag = u16;

/// An unreachable cons. In other words, it is a "null" pointer but not `null`
/// in Scheme.
///
/// This value means:
///
/// - In `car`, its cons is moved already on garbage collection.
/// - In `cdr`, nothing.
pub(crate) const NEVER: Cons = Cons::new(u64::MAX);

const TAG_SIZE: usize = Tag::BITS as usize;
const TAG_MASK: u64 = Tag::MAX as u64;

/// A cons.
#[derive(Clone, Copy, Debug)]
pub struct Cons(u64);

impl Cons {
    /// Creates a cons from a memory address on heap.
    #[inline]
    pub const fn new(index: u64) -> Self {
        Self::r#box(index << TAG_SIZE)
    }

    /// Returns a memory address on heap.
    #[inline]
    pub const fn index(self) -> usize {
        (self.unbox() >> TAG_SIZE) as _
    }

    /// Returns a tag.
    #[inline]
    pub const fn tag(self) -> Tag {
        (self.unbox() & TAG_MASK) as _
    }

    /// Sets a tag.
    #[inline]
    pub const fn set_tag(self, tag: Tag) -> Self {
        Self::r#box(self.unbox() & !TAG_MASK | (tag as u64 & TAG_MASK))
    }

    #[inline]
    const fn r#box(value: u64) -> Self {
        Self(value_inner::box_cons(value))
    }

    #[inline]
    const fn unbox(self) -> u64 {
        value_inner::unbox_cons(self.0)
    }

    #[inline]
    pub(crate) const fn index_eq(&self, other: Self) -> bool {
        self.index() == other.index()
    }

    #[inline]
    pub(crate) const unsafe fn from_raw(raw: u64) -> Self {
        Self(raw)
    }

    #[inline]
    pub(crate) const fn to_raw(self) -> u64 {
        self.0
    }
}

impl PartialEq for Cons {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.index_eq(*other)
    }
}

impl Eq for Cons {}

impl TryFrom<Value> for Cons {
    type Error = Error;

    #[inline]
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        value.to_cons().ok_or(Error::ConsExpected)
    }
}

impl Display for Cons {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        if *self == NEVER {
            return write!(formatter, "!");
        }

        write!(formatter, "c{:x}", self.index())?;

        if self.tag() > 0 {
            write!(formatter, ":{}", self.tag())?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tag() {
        let cons = unsafe { Cons::new(42) };

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
        assert_eq!(unsafe { Cons::new(42) }.set_tag(2).set_tag(1).tag(), 1);
    }

    #[test]
    fn set_too_large_tag() {
        let cons = unsafe { Cons::new(0) }.set_tag(Tag::MAX);

        assert_eq!(cons.index(), 0);
        assert_eq!(cons.tag(), TAG_MASK as Tag);
    }
}
