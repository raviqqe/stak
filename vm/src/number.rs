#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Number(u64);

impl Number {
    pub const fn new(number: u64) -> Self {
        Self(number)
    }

    pub const fn to_usize(self) -> usize {
        self.0 as usize
    }
}
