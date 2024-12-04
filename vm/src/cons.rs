use crate::{value::Value, Error};
use cfg_exif::feature;
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
pub const NEVER: Cons = unsafe { Cons::new(0) }.set_tag(Tag::MAX);

const TAG_SIZE: usize = Tag::BITS as usize;
const TAG_MASK: u64 = Tag::MAX as u64;

/// A cons.
#[derive(Clone, Copy, Debug)]
pub struct Cons(u64);

impl Cons {
    /// Creates a cons from a memory address on heap.
    ///
    /// # Safety
    ///
    /// The given index must be valid in a heap passed to
    /// [`Memory::new`](super::Memory::new).
    pub const unsafe fn new(index: u64) -> Self {
        Self::r#box(index << TAG_SIZE)
    }

    /// Returns a memory address on heap.
    pub const fn index(self) -> usize {
        (self.unbox() >> TAG_SIZE) as _
    }

    /// Returns a tag.
    pub const fn tag(self) -> Tag {
        (self.unbox() & TAG_MASK) as _
    }

    /// Sets a tag.
    pub const fn set_tag(self, tag: Tag) -> Self {
        Self::r#box(self.unbox() & !TAG_MASK | (tag as u64 & TAG_MASK))
    }

    const fn r#box(value: u64) -> Self {
        Self(feature!(if ("float") {
            nonbox::f64::u64::box_unsigned(value)
        } else {
            value << 1
        }))
    }

    const fn unbox(self) -> u64 {
        feature!(if ("float") {
            nonbox::f64::u64::unbox_unsigned(self.0).unwrap()
        } else {
            self.0 >> 1
        })
    }

    pub(crate) const fn raw_eq(self, cons: Self) -> bool {
        self.0 == cons.0
    }

    pub(crate) const unsafe fn from_raw(raw: u64) -> Self {
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
        if self.raw_eq(NEVER) {
            write!(formatter, "!")?;
        } else {
            write!(formatter, "c{:x}", self.index())?;
        }

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
