use super::instruction::Instruction;
use alloc::{string::String, vec::Vec};
use core::fmt::{self, Display, Formatter};

#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    symbols: Vec<String>,
    instructions: Vec<Instruction>,
}

impl Program {
    pub fn new(symbols: Vec<String>, instructions: Vec<Instruction>) -> Self {
        Self {
            symbols,
            instructions,
        }
    }

    pub fn symbols(&self) -> &[String] {
        &self.symbols
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }
}

impl Display for Program {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        writeln!(formatter, "symbols: {:?}", self.symbols)?;

        for instruction in &self.instructions {
            writeln!(formatter, "{}", instruction)?;
        }

        Ok(())
    }
}
