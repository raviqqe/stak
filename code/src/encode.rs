use crate::{Instruction, Program};
use alloc::vec;
use alloc::vec::Vec;

pub fn encode(program: &Program) -> Vec<u8> {
    let codes = vec![];

    for instruction in program.instructions() {
        match instruction {
            Instruction::Apply(operand, bool) => todo!(),
            Instruction::Set(operand) => todo!(),
            Instruction::Get(operand) => todo!(),
            Instruction::Constant(number) => todo!(),
            Instruction::If(r#then, r#else) => todo!(),
            Instruction::Halt => todo!(),
        }
    }

    codes
}
