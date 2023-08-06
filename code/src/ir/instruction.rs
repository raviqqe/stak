use crate::Operand;
#[cfg(feature = "alloc")]
use alloc::vec::Vec;

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Call(Operand),
    Set(Operand),
    Get(Operand),
    Constant(Operand),
    #[cfg(feature = "alloc")]
    If(Vec<Instruction>),
    #[cfg(feature = "alloc")]
    Closure(u64, Vec<Instruction>),
    Skip(u64),
}

impl Instruction {
    pub const CALL: u8 = 0;
    pub const SET: u8 = 1;
    pub const GET: u8 = 2;
    pub const CONSTANT: u8 = 3;
    pub const IF: u8 = 4;
    pub const CLOSURE: u8 = 5;
    pub const SKIP: u8 = 6;
}
