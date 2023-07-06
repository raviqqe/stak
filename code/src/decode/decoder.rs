use crate::{encode::INTEGER_BASE, Error, Instruction, Operand, Program};
use alloc::{string::String, vec, vec::Vec};

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
        let mut instructions = vec![];

        while let Some(instruction) = self.decode_byte() {
            match instruction {
                Instruction::APPLY_TAIL => todo!(),
                Instruction::APPLY => todo!(),
                Instruction::SET => {
                    let operand = self.decode_operand()?;
                    instructions.push(Instruction::Set(operand));
                }
                Instruction::GET => todo!(),
                Instruction::CONSTANT => todo!(),
                Instruction::IF => todo!(),
                _ => return Err(Error::IllegalInstruction),
            }
        }

        Ok(instructions)
    }

    fn decode_operand(&mut self) -> Result<Operand, Error> {
        let integer = self.decode_integer().ok_or(Error::MissingOperand)? as i64;

        Ok(if integer < 0 {
            Operand::Global(-integer as u64)
        } else {
            Operand::Local(integer as u64)
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
        let &byte = self.codes.get(self.index)?;
        self.index += 1;
        Some(byte)
    }
}
