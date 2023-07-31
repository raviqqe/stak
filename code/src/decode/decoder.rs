use crate::{
    Error, Instruction, Operand, Program, INSTRUCTION_BITS, INSTRUCTION_MASK, INTEGER_BASE,
    SHORT_INTEGER_BASE,
};
use alloc::{string::String, vec, vec::Vec};
use core::mem::{replace, take};

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
        let mut symbols = vec![];
        let mut symbol = vec![];

        loop {
            match self.decode_byte().ok_or(Error::EndOfInput)? {
                character @ (b',' | b';') => {
                    symbol.reverse();
                    symbols.push(String::from_utf8(take(&mut symbol))?);

                    if character == b';' {
                        return Ok(symbols);
                    }
                }
                character => symbol.push(character),
            }
        }
    }

    fn decode_instructions(&mut self) -> Result<Vec<Instruction>, Error> {
        let mut instruction_lists = vec![];
        let mut instructions = vec![];

        while let Some(byte) = self.decode_byte() {
            let integer = byte >> INSTRUCTION_BITS;
            let instruction = byte & INSTRUCTION_MASK;

            match instruction {
                Instruction::RETURN_CALL => {
                    instructions.reverse();
                    instruction_lists.push(take(&mut instructions));
                    instructions.push(Instruction::Call(self.decode_operand(integer)?, true))
                }
                Instruction::CALL => {
                    instructions.push(Instruction::Call(self.decode_operand(integer)?, false))
                }
                Instruction::CLOSURE => {
                    let body = replace(
                        &mut instructions,
                        instruction_lists.pop().ok_or(Error::MissingClosureBody)?,
                    );

                    instructions.push(Instruction::Closure(
                        self.decode_integer().ok_or(Error::MissingOperand)?,
                        body,
                    ));
                }
                Instruction::SET => {
                    instructions.push(Instruction::Set(self.decode_operand(integer)?))
                }
                Instruction::GET => {
                    instructions.push(Instruction::Get(self.decode_operand(integer)?))
                }
                Instruction::CONSTANT => {
                    instructions.push(Instruction::Constant(self.decode_operand(integer)?))
                }
                Instruction::IF => {
                    instructions.reverse();
                    let then = take(&mut instructions);

                    instructions.push(Instruction::If(
                        then,
                        instruction_lists.pop().ok_or(Error::MissingElseBranch)?,
                    ));
                }
                _ => return Err(Error::IllegalInstruction),
            }
        }

        instructions.reverse();

        Ok(instructions)
    }

    fn decode_operand(&mut self, integer: u8) -> Result<Operand, Error> {
        let integer = self.decode_integer(integer).ok_or(Error::MissingOperand)?;
        let global = integer & 1 == 0;
        let index = integer >> 1;

        Ok(if global {
            Operand::Symbol(index)
        } else {
            Operand::Integer(index)
        })
    }

    fn decode_integer(&mut self, rest: u8) -> Option<u64> {
        let mut y = 0;

        while {
            y *= INTEGER_BASE;
            let x = self.decode_byte()? as i8;

            y += (if x < 0 { -1 } else { 1 } * x) as u64;

            x < 0
        } {}

        Some(y * SHORT_INTEGER_BASE + rest)
    }

    fn decode_byte(&mut self) -> Option<u8> {
        if self.index >= self.codes.len() {
            return None;
        }

        let byte = self.codes[self.index];
        self.index += 1;
        Some(byte)
    }
}
