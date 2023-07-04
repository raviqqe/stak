#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    Apply,
    Set,
    Get,
    Constant,
    If,
    Halt,
}

impl Instruction {
    pub const APPLY: u8 = Self::Apply as _;
    pub const SET: u8 = Self::Set as _;
    pub const GET: u8 = Self::Get as _;
    pub const CONSTANT: u8 = Self::Constant as _;
    pub const IF: u8 = Self::If as _;
    pub const HALT: u8 = Self::Halt as _;
}
