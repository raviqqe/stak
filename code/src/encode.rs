use crate::{Instruction, Program};
use alloc::vec;
use alloc::vec::Vec;

const BASE: u64 = i8::MAX as u64 + 1;

pub fn encode(program: &Program) -> Vec<u8> {
    let mut codes = vec![];

    encode_instructions(&mut codes, program.instructions());

    codes
}

// TODO Use short encoding for instruction operands.
fn encode_instructions(codes: &mut Vec<u8>, instructions: &[Instruction]) {
    for instruction in instructions {
        match instruction {
            Instruction::Apply(operand, bool) => todo!(),
            Instruction::Set(operand) => todo!(),
            Instruction::Get(operand) => todo!(),
            Instruction::Constant(number) => {
                encode_u64(codes, *number);
                codes.push(Instruction::CONSTANT)
            }
            Instruction::If(r#then, r#else) => todo!(),
            Instruction::Halt => codes.push(Instruction::HALT),
        }
    }
}

// Base 128 encoding
fn encode_u64(codes: &mut Vec<u8>, mut number: u64) {
    let rest = false;

    while {
        let part = number % BASE;

        codes.push((if rest { -1 } else { 1 } * part as i64) as u8);

        number /= BASE;

        rest = true;

        number != 0
    } {}
}
