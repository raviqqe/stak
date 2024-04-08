mod instruction;
mod operand;
#[cfg(feature = "alloc")]
mod program;

pub use instruction::{Code, Instruction};
pub use operand::Operand;
#[cfg(feature = "alloc")]
pub use program::Program;
