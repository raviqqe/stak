use super::instruction::Instruction;
use alloc::{borrow::ToOwned, string::String, vec::Vec};
use core::fmt::{self, Display, Formatter};

const ESCAPED_SIGNS: &[&str] = &["\\", "+", "*", "_"];

/// A program on a virtual machine.
#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    symbols: Vec<String>,
    instructions: Vec<Instruction>,
}

impl Program {
    /// Creates a program.
    pub fn new(symbols: Vec<String>, instructions: Vec<Instruction>) -> Self {
        Self {
            symbols,
            instructions,
        }
    }

    /// Returns symbols in a program.
    pub fn symbols(&self) -> &[String] {
        &self.symbols
    }

    /// Returns instructions in a program.
    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }
}

impl Display for Program {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        writeln!(formatter, "# symbols")?;

        for symbol in &self.symbols {
            let mut symbol = symbol.clone();

            for sign in ESCAPED_SIGNS {
                symbol = symbol.replace(sign, &("\\".to_owned() + sign));
            }

            writeln!(formatter, "- {symbol}")?;
        }

        write!(formatter, "# instructions")?;
        write!(
            formatter,
            "{}",
            Instruction::display_slice(&self.instructions)
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Operand;
    use insta::assert_snapshot;
    use alloc::{format, vec};

    #[test]
    fn display_symbols() {
        assert_snapshot!(Program::new(vec!["foo".into(), "bar".into()], vec![],));
    }

    #[test]
    fn display_symbols_with_special_signs() {
        for &sign in ESCAPED_SIGNS {
            assert_snapshot!(
                sign.replace('*', "star"),
                Program::new(vec![format!("{}", sign)], vec![])
            );
        }
    }

    #[test]
    fn display_if() {
        assert_snapshot!(Program::new(
            vec![],
            vec![Instruction::If(vec![Instruction::Constant(
                Operand::Integer(42)
            )])],
        ));
    }

    #[test]
    fn display_closure() {
        assert_snapshot!(Program::new(
            vec![],
            vec![Instruction::Close(
                42,
                vec![Instruction::Constant(Operand::Integer(2045))],
            )],
        ));
    }

    #[test]
    fn display_closure_with_if() {
        assert_snapshot!(Program::new(
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
