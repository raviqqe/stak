use crate::{encode::INTEGER_BASE, Error, Instruction, Operand, Program};
use alloc::{string::String, vec, vec::Vec};
use core::mem::take;

pub struct Decoder<'a> {
    codes: &'a [u8],
    index: usize,
}

impl<'a> Decoder<'a> {
    pub fn new(codes: &'a [u8]) -> Self {
        Self { codes, index: 0 }
    }

    pub fn decode(&mut self) -> Result<Program, Error> {
        Ok(Program::new(
            self.decode_symbols()?,
            self.decode_instructions()?,
        ))
    }

    fn decode_symbols(&mut self) -> Result<Vec<String>, Error> {
        // TODO
        Ok(vec![])
    }

    fn decode_instructions(&mut self) -> Result<Vec<Instruction>, Error> {
        let mut instruction_lists = vec![];
        let mut instructions = vec![];

        while let Some(instruction) = self.decode_byte() {
            match instruction {
                Instruction::TAIL_APPLY => {
                    instruction_lists.push(take(&mut instructions));
                    instructions.push(Instruction::Apply(self.decode_operand()?, true))
                }
                Instruction::APPLY => {
                    instructions.push(Instruction::Apply(self.decode_operand()?, false))
                }
                Instruction::SET => instructions.push(Instruction::Set(self.decode_operand()?)),
                Instruction::GET => instructions.push(Instruction::Get(self.decode_operand()?)),
                Instruction::CONSTANT => instructions.push(Instruction::Constant(
                    self.decode_integer().ok_or(Error::MissingOperand)?,
                )),
                Instruction::IF => {
                    let then = take(&mut instructions);

                    instructions.push(Instruction::If(
                        then,
                        instruction_lists.pop().ok_or(Error::MissingElseBranch)?,
                    ));
                }
                _ => return Err(Error::IllegalInstruction),
            }
        }

        Ok(instructions)
    }

    fn decode_operand(&mut self) -> Result<Operand, Error> {
        let integer = self.decode_integer().ok_or(Error::MissingOperand)?;
        let global = integer & 1 == 0;
        let index = integer >> 1;

        Ok(if global {
            Operand::Global(index)
        } else {
            Operand::Local(index)
        })
    }

    fn decode_integer(&mut self) -> Option<u64> {
        let mut y = 0;

        while {
            y *= INTEGER_BASE;
            let x = self.decode_byte()? as i8;

            y += (if x < 0 { -1 } else { 1 } * x) as u64;

            x < 0
        } {}

        Some(y)
    }

    fn decode_byte(&mut self) -> Option<u8> {
        if self.index >= self.codes.len() {
            return None;
        }

        let byte = self.codes[self.codes.len() - 1 - self.index];
        self.index += 1;
        Some(byte)
    }
}
