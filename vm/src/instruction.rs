use crate::Tag;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Instruction {
    Constant,
    Get,
    Set,
    If,
    Nop,
    Call,
}

impl Instruction {
    pub const CONSTANT: Tag = Self::Constant as _;
    pub const GET: Tag = Self::Get as _;
    pub const SET: Tag = Self::Set as _;
    pub const IF: Tag = Self::If as _;
    pub const NOP: Tag = Self::Nop as _;
    pub const CALL: Tag = Self::Call as _;
}
