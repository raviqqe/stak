use crate::Operand;
use alloc::vec::Vec;

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Apply(Operand, bool),
    Set(Operand),
    Get(Operand),
    Constant(u64),
    If(Vec<Instruction>, Vec<Instruction>),
}

impl Instruction {
    pub const TAIL_APPLY: u8 = 0;
    pub const APPLY: u8 = 1;
    pub const SET: u8 = 2;
    pub const GET: u8 = 3;
    pub const CONSTANT: u8 = 4;
    pub const IF: u8 = 5;
}
