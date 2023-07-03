#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Cons(u64);

impl Cons {
    pub const fn new(index: u64) -> Self {
        Self(index)
    }

    pub const fn index(self) -> u64 {
        self.0
    }
}
