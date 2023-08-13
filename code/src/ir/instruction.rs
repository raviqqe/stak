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

impl Display for Instruction {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Call(operand) => write!(formatter, "call {}", operand),
            Self::Set(operand) => write!(formatter, "set {}", operand),
            Self::Get(operand) => write!(formatter, "get {}", operand),
            Self::Constant(operand) => write!(formatter, "constant {}", operand),
            #[cfg(feature = "alloc")]
            Self::If(instructions) => {
                writeln!(formatter, "if")?;
                format_instructions(&instructions, formatter)
            }
            #[cfg(feature = "alloc")]
            Self::Closure(index, instructions) => todo!(),
            Self::Skip(index) => todo!(),
        }
    }
}

pub fn format_instructions(instructions: &[Instruction], formatter: &mut Formatter) -> fmt::Result {
    for instruction in &self.instructions {
        writeln!(formatter, "{}", instruction)?;
    }

    Ok(())
}
