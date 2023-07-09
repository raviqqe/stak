use crate::Operand;
use alloc::vec::Vec;

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Call(Operand, bool),
    Close(u64),
    Set(Operand),
    Get(Operand),
    Constant(u64),
    If(Vec<Instruction>, Vec<Instruction>),
}

impl Instruction {
    pub const RETURN_CALL: u8 = 0;
    pub const CALL: u8 = 1;
    pub const CLOSE: u8 = 2;
    pub const SET: u8 = 3;
    pub const GET: u8 = 4;
    pub const CONSTANT: u8 = 5;
    pub const IF: u8 = 6;
}
