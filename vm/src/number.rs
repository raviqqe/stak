#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Number(u64);

impl Number {
    pub fn new(number: u64) -> Self {
        Self(number)
    }

    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}
