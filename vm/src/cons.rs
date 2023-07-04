use core::fmt::{self, Display, Formatter};

const TAG_MASK: u64 = 0b1111;
const TAG_SIZE: usize = TAG_MASK.count_ones() as usize;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Cons(u64);

impl Cons {
    pub const fn new(index: u64) -> Self {
        Self(index << TAG_SIZE)
    }

    pub const fn index(self) -> usize {
        (self.0 >> TAG_SIZE) as usize
    }

    pub const fn tag(self) -> u8 {
        (self.0 & TAG_MASK) as u8
    }

    pub const fn set_tag(self, tag: u8) -> Self {
        Self(self.0 | tag as u64 & TAG_MASK)
    }
}

impl Display for Cons {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "c{:x}:{}", self.index(), self.tag())
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

        let cons = cons.set_tag(3);

        assert_eq!(cons.index(), 42);
        assert_eq!(cons.tag(), 3);
    }
}
