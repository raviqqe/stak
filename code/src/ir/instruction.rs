use crate::Operand;
#[cfg(feature = "alloc")]
use alloc::vec::Vec;

/// An instruction.
#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    /// A `constant` instruction.
    Constant(Operand),
    /// A `get` instruction.
    Get(Operand),
    /// A `set` instruction.
    Set(Operand),
    /// An `if` instruction.
    #[cfg(feature = "alloc")]
    If(Vec<Instruction>),
    /// A `nop` instruction.
    Nop(u64),
    /// A `call` instruction.
    Call(u64, Operand),
    /// A `close` instruction.
    ///
    /// It is used only for encoding.
    #[cfg(feature = "alloc")]
    Close(u64, Vec<Instruction>),
    /// A `skip` instruction.
    ///
    /// It is used only for encoding.
    Skip(u64),
}

impl Instruction {
    pub const CONSTANT: u8 = 0;
    pub const GET: u8 = 1;
    pub const SET: u8 = 2;
    pub const IF: u8 = 3;
    pub const NOP: u8 = 4;
    pub const CALL: u8 = 5;
    pub const CLOSE: u8 = 6;
    pub const SKIP: u8 = 7;
}

#[cfg(feature = "alloc")]
mod display {
    use super::*;
    use core::fmt::{self, Display, Formatter};

    /// Displays instructions in a slice.
    impl Instruction {
        pub fn display_slice(instructions: &[Self]) -> impl Display + '_ {
            DisplayInstructionList::new(instructions, 0)
        }
    }

    struct DisplayInstruction<'a> {
        instruction: &'a Instruction,
        #[allow(unused)]
        indent: usize,
    }

    impl<'a> DisplayInstruction<'a> {
        fn new(instruction: &'a Instruction, indent: usize) -> Self {
            Self {
                instruction,
                indent,
            }
        }
    }

    impl<'a> Display for DisplayInstruction<'a> {
        fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
            let indent = self.indent + 1;

            write!(formatter, "- ")?;

            match self.instruction {
                Instruction::Call(arity, operand) => {
                    write!(formatter, "call {arity} {operand}")
                }
                Instruction::Set(operand) => write!(formatter, "set {operand}"),
                Instruction::Get(operand) => write!(formatter, "get {operand}"),
                Instruction::Constant(operand) => write!(formatter, "constant {operand}"),
                Instruction::If(instructions) => {
                    write!(formatter, "if")?;
                    write!(
                        formatter,
                        "{}",
                        DisplayInstructionList::new(instructions, indent)
                    )
                }
                Instruction::Nop(operand) => write!(formatter, "nop {operand}"),
                Instruction::Close(arity, instructions) => {
                    write!(formatter, "close {arity}")?;
                    write!(
                        formatter,
                        "{}",
                        DisplayInstructionList::new(instructions, indent)
                    )
                }
                Instruction::Skip(count) => write!(formatter, "skip {count}"),
            }
        }
    }

    struct DisplayInstructionList<'a> {
        instructions: &'a [Instruction],
        indent: usize,
    }

    impl<'a> DisplayInstructionList<'a> {
        fn new(instructions: &'a [Instruction], indent: usize) -> Self {
            Self {
                instructions,
                indent,
            }
        }
    }

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
}
