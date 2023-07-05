use alloc::vec::Vec;

use crate::Operand;

#[derive(Debug)]
pub enum Instruction {
    Apply(Operand, bool),
    Set(Operand),
    Get(Operand),
    Constant(u64),
    If(Vec<Instruction>, Vec<Instruction>),
    Halt,
}
