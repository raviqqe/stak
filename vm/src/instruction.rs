#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    Apply,
    Set,
    Get,
    Constant,
    If,
}

impl Instruction {
    pub const APPLY: u64 = Self::Apply as u64;
    pub const SET: u64 = Self::Set as u64;
    pub const GET: u64 = Self::Get as u64;
    pub const CONSTANT: u64 = Self::Constant as u64;
    pub const IF: u64 = Self::If as u64;
}
