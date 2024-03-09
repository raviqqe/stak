use crate::{
    Error, Instruction, Operand, Program, INSTRUCTION_BITS, INSTRUCTION_MASK, INTEGER_BASE,
    SHORT_INTEGER_BASE, SYMBOL_SEPARATOR, SYMBOL_TERMINATOR,
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
        let mut symbols = (0..self.decode_integer().ok_or(Error::MissingInteger)?)
            .map(|_| Default::default())
            .collect();
        let mut symbol = vec![];
        let mut byte = self.decode_byte().ok_or(Error::EndOfInput)?;

        if byte == SYMBOL_TERMINATOR {
            return Ok(symbols);
        }

        loop {
            if matches!(byte, SYMBOL_SEPARATOR | SYMBOL_TERMINATOR) {
                symbol.reverse();
                symbols.push(String::from_utf8(take(&mut symbol))?);

                if byte == SYMBOL_TERMINATOR {
                    symbols.reverse();
                    return Ok(symbols);
                }
            } else {
                symbol.push(byte)
            }

            byte = self.decode_byte().ok_or(Error::EndOfInput)?;
        }
    }

    fn decode_instructions(&mut self) -> Result<Vec<Instruction>, Error> {
        let mut instruction_lists = vec![];
        let mut instructions = vec![];

        while let Some((instruction, r#return, integer)) = self.decode_instruction()? {
            let operand = Self::decode_operand(integer);

            let instruction = match instruction {
                Instruction::CALL => Instruction::Call(
                    integer,
                    Self::decode_operand(self.decode_integer().ok_or(Error::MissingOperand)?),
                ),
                Instruction::SET => Instruction::Set(operand),
                Instruction::GET => Instruction::Get(operand),
                Instruction::CONSTANT => Instruction::Constant(operand),
                Instruction::IF => {
                    let instruction = Instruction::If(take({
                        instructions.reverse();
                        &mut instructions
                    }));

                    instructions = instruction_lists.pop().ok_or(Error::MissingElseBranch)?;

                    instruction
                }
                Instruction::CLOSE => Instruction::Close(
                    integer,
                    replace(
                        {
                            instructions.reverse();
                            &mut instructions
                        },
                        instruction_lists.pop().ok_or(Error::MissingClosureBody)?,
                    ),
                ),
                Instruction::SKIP => Instruction::Skip(integer),
                Instruction::NOP => Instruction::Nop(integer),
                _ => return Err(Error::IllegalInstruction),
            };

            if r#return {
                instruction_lists.push(take(&mut instructions));
            }

            instructions.push(instruction);
        }

        instructions.reverse();

        Ok(instructions)
    }

    fn decode_instruction(&mut self) -> Result<Option<(u8, bool, u64)>, Error> {
        let Some(byte) = self.decode_byte() else {
            return Ok(None);
        };

        let instruction = byte & INSTRUCTION_MASK;

        Ok(Some((
            instruction >> 1,
            instruction & 1 != 0,
            self.decode_short_integer(byte >> INSTRUCTION_BITS)
                .ok_or(Error::MissingOperand)?,
        )))
    }

    fn decode_operand(integer: u64) -> Operand {
        let index = integer >> 1;

        if integer & 1 == 0 {
            Operand::Symbol(index)
        } else {
            Operand::Integer(index)
        }
    }

    fn decode_integer(&mut self) -> Option<u64> {
        let byte = self.decode_byte()?;
        self.decode_integer_rest(byte, INTEGER_BASE)
    }

    fn decode_short_integer(&mut self, rest: u8) -> Option<u64> {
        self.decode_integer_rest(rest, SHORT_INTEGER_BASE)
    }

    fn decode_integer_rest(&mut self, rest: u8, base: u64) -> Option<u64> {
        let mut x = rest;
        let mut y = 0;

        while x & 1 != 0 {
            y *= INTEGER_BASE;
            x = self.decode_byte()?;
            y += (x >> 1) as u64;
        }

        Some(y * base + (rest >> 1) as u64)
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
