use crate::Operand;
#[cfg(feature = "alloc")]
use alloc::vec::Vec;
use core::fmt::{self, Display, Formatter};

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Call(u64, Operand),
    Set(Operand),
    Get(Operand),
    Constant(Operand),
    #[cfg(feature = "alloc")]
    If(Vec<Instruction>),
    Nop(u64),
    #[cfg(feature = "alloc")]
    Close(u64, Vec<Instruction>),
    Skip(u64),
}

impl Instruction {
    pub const CALL: u8 = 0;
    pub const SET: u8 = 1;
    pub const GET: u8 = 2;
    pub const CONSTANT: u8 = 3;
    pub const IF: u8 = 4;
    pub const NOP: u8 = 5;
    pub const CLOSE: u8 = 6;
    pub const SKIP: u8 = 7;
}

pub(crate) struct DisplayInstruction<'a> {
    instruction: &'a Instruction,
    #[allow(unused)]
    indent: usize,
}

impl<'a> DisplayInstruction<'a> {
    pub fn new(instruction: &'a Instruction, indent: usize) -> Self {
        Self {
            instruction,
            indent,
        }
    }
}

#[cfg(feature = "alloc")]
impl<'a> Display for DisplayInstruction<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        let indent = self.indent + 1;

        match self.instruction {
            Instruction::Call(count, operand) => write!(formatter, "call {} {}", count, operand),
            Instruction::Set(operand) => write!(formatter, "set {}", operand),
            Instruction::Get(operand) => write!(formatter, "get {}", operand),
            Instruction::Constant(operand) => write!(formatter, "constant {}", operand),
            Instruction::If(instructions) => {
                write!(formatter, "if")?;
                write!(
                    formatter,
                    "{}",
                    DisplayInstructionList::new(instructions, indent)
                )
            }
            Instruction::Nop(operand) => write!(formatter, "nop {}", operand),
            Instruction::Close(arity, instructions) => {
                write!(formatter, "closure {}", arity)?;
                write!(
                    formatter,
                    "{}",
                    DisplayInstructionList::new(instructions, indent)
                )
            }
            Instruction::Skip(count) => write!(formatter, "skip {}", count),
        }
    }
}

pub(crate) struct DisplayInstructionList<'a> {
    instructions: &'a [Instruction],
    indent: usize,
}

impl<'a> DisplayInstructionList<'a> {
    pub fn new(instructions: &'a [Instruction], indent: usize) -> Self {
        Self {
            instructions,
            indent,
        }
    }
}

#[cfg(feature = "alloc")]
impl<'a> Display for DisplayInstructionList<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        for instruction in self.instructions {
            writeln!(formatter)?;

            for _ in 0..self.indent {
                write!(formatter, "  ")?
            }

            write!(
                formatter,
                "{}",
                DisplayInstruction::new(instruction, self.indent)
            )?;
        }

        Ok(())
    }
}
