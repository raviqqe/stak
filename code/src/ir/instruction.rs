use crate::Operand;
#[cfg(feature = "alloc")]
use alloc::vec::Vec;
use core::fmt::{self, Display, Formatter};

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Call(Operand),
    Set(Operand),
    Get(Operand),
    Constant(Operand),
    #[cfg(feature = "alloc")]
    If(Vec<Instruction>),
    #[cfg(feature = "alloc")]
    Closure(u64, Vec<Instruction>),
    Skip(u64),
}

impl Instruction {
    pub const CALL: u8 = 0;
    pub const SET: u8 = 1;
    pub const GET: u8 = 2;
    pub const CONSTANT: u8 = 3;
    pub const IF: u8 = 4;
    pub const CLOSURE: u8 = 5;
    pub const SKIP: u8 = 6;
}

pub(crate) struct DisplayInstruction<'a> {
    instruction: &'a Instruction,
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

impl<'a> Display for DisplayInstruction<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        let indent = self.indent + 1;

        match self.instruction {
            Instruction::Call(operand) => write!(formatter, "call {}", operand),
            Instruction::Set(operand) => write!(formatter, "set {}", operand),
            Instruction::Get(operand) => write!(formatter, "get {}", operand),
            Instruction::Constant(operand) => write!(formatter, "constant {}", operand),
            #[cfg(feature = "alloc")]
            Instruction::If(instructions) => {
                writeln!(formatter, "if")?;
                write!(
                    formatter,
                    "{}",
                    DisplayInstructionList::new(instructions, indent)
                )
            }
            #[cfg(feature = "alloc")]
            Instruction::Closure(arity, instructions) => {
                writeln!(formatter, "closure {}", arity)?;
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

impl<'a> Display for DisplayInstructionList<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        for (index, instruction) in self.instructions.iter().enumerate() {
            for _ in 0..self.indent {
                write!(formatter, "  ")?
            }

            write!(
                formatter,
                "{}",
                DisplayInstruction::new(instruction, self.indent)
            )?;

            if index < self.instructions.len() - 1 {
                writeln!(formatter)?;
            }
        }

        Ok(())
    }
}
