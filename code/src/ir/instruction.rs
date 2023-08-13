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
    pub fn body(&self) -> &[Instruction] {
        match self {
            Self::Call(operand) => write!(formatter, "call {}", operand),
            Self::Set(operand) => write!(formatter, "set {}", operand),
            Self::Get(operand) => write!(formatter, "get {}", operand),
            Self::Constant(operand) => write!(formatter, "constant {}", operand),
            #[cfg(feature = "alloc")]
            Self::If(instructions) => write!(formatter, "if"),
            #[cfg(feature = "alloc")]
            Self::Closure(arity, instructions) => writeln!(formatter, "closure {}", arity),
            Self::Skip(count) => write!(formatter, "skip {}", count),
        }
    }
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

impl Display for Instruction {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Call(operand) => write!(formatter, "call {}", operand),
            Self::Set(operand) => write!(formatter, "set {}", operand),
            Self::Get(operand) => write!(formatter, "get {}", operand),
            Self::Constant(operand) => write!(formatter, "constant {}", operand),
            #[cfg(feature = "alloc")]
            Self::If(instructions) => write!(formatter, "if"),
            #[cfg(feature = "alloc")]
            Self::Closure(arity, instructions) => writeln!(formatter, "closure {}", arity),
            Self::Skip(count) => write!(formatter, "skip {}", count),
        }
    }
}

pub struct DisplayInstructionList<'a> {
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
            writeln!(formatter, "{}", instruction)?;

            if index < self.instructions.len() - 1 {
                writeln!(formatter)?;

                for _ in 0..self.indent {
                    write!(formatter, "  ")?
                }
            }
        }

        Ok(())
    }
}
