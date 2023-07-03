#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Number(u64);

impl Number {
    pub fn new(index: u64) -> Self {
        Self(index)
    }

    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}
