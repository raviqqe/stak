#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    Apply,
    Set,
    Get,
    Constant,
    If,
}

impl Instruction {
    pub const APPLY: u8 = Self::Apply as u8;
    pub const SET: u8 = Self::Set as u8;
    pub const GET: u8 = Self::Get as u8;
    pub const CONSTANT: u8 = Self::Constant as u8;
    pub const IF: u8 = Self::If as u8;
}
