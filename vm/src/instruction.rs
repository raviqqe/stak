#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    Call,
    Set,
    Get,
    Constant,
    If,
    Halt,
}

impl Instruction {
    pub const CALL: u8 = Self::Call as _;
    pub const SET: u8 = Self::Set as _;
    pub const GET: u8 = Self::Get as _;
    pub const CONSTANT: u8 = Self::Constant as _;
    pub const IF: u8 = Self::If as _;
    pub const HALT: u8 = Self::Halt as _;
}
