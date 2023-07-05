use crate::{Instruction, Program};
use alloc::vec;
use alloc::vec::Vec;

pub fn encode(program: &Program) -> Vec<u8> {
    let mut codes = vec![];

    encode_instructions(&mut codes, program.instructions());

    codes
}

fn encode_instructions(codes: &mut Vec<u8>, instructions: &[Instruction]) {
    for instruction in instructions {
        match instruction {
            Instruction::Apply(operand, bool) => todo!(),
            Instruction::Set(operand) => todo!(),
            Instruction::Get(operand) => todo!(),
            Instruction::Constant(number) => todo!(),
            Instruction::If(r#then, r#else) => todo!(),
            Instruction::Halt => codes.push(Instruction::HALT),
        }
    }
}
