use crate::Operand;
use alloc::vec::Vec;

#[derive(Debug)]
pub enum Instruction {
    Apply(Operand, bool),
    Set(Operand),
    Get(Operand),
    Constant(u64),
    If(Vec<Instruction>, Vec<Instruction>),
    Halt,
}

impl Instruction {
    pub const APPLY: u8 = 0;
    pub const SET: u8 = 1;
    pub const GET: u8 = 2;
    pub const CONSTANT: u8 = 3;
    pub const IF: u8 = 4;
    pub const HALT: u8 = 5;
}
