use super::instruction::{DisplayInstructionList, Instruction};
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
        writeln!(formatter, "symbols:")?;

        for symbol in &self.symbols {
            writeln!(formatter, "- {}", symbol)?;
        }

        write!(formatter, "instructions:")?;
        write!(
            formatter,
            "{}",
            DisplayInstructionList::new(&self.instructions, 0)
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Operand;
    use insta::assert_display_snapshot;
    use std::{format, vec};

    #[test]
    fn display_symbols() {
        assert_display_snapshot!(Program::new(vec!["foo".into(), "bar".into()], vec![],));
    }

    #[test]
    fn display_if() {
        assert_display_snapshot!(Program::new(
            vec![],
            vec![Instruction::If(vec![Instruction::Constant(
                Operand::Integer(42)
            )])],
        ));
    }

    #[test]
    fn display_closure() {
        assert_display_snapshot!(Program::new(
            vec![],
            vec![Instruction::Close(
                42,
                vec![Instruction::Constant(Operand::Integer(2045))],
            )],
        ));
    }

    #[test]
    fn display_closure_with_if() {
        assert_display_snapshot!(Program::new(
            vec![],
            vec![
                Instruction::Constant(Operand::Integer(0)),
                Instruction::Constant(Operand::Integer(1)),
                Instruction::Close(
                    42,
                    vec![
                        Instruction::Constant(Operand::Integer(2)),
                        Instruction::If(vec![
                            Instruction::Constant(Operand::Integer(3)),
                            Instruction::Constant(Operand::Integer(4)),
                        ]),
                        Instruction::Constant(Operand::Integer(5)),
                        Instruction::Constant(Operand::Integer(6)),
                    ],
                ),
                Instruction::Constant(Operand::Integer(7)),
                Instruction::Constant(Operand::Integer(8)),
            ],
        ));
    }
}
