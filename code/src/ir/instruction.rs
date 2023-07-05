use alloc::vec::Vec;

use crate::Operand;

#[derive(Debug)]
pub enum Instruction {
    Apply(Operand, bool),
    Set(Operand),
    Get(Operand),
    Constant(Operand),
    If(Vec<Instruction>, Vec<Instruction>),
    Halt,
}
