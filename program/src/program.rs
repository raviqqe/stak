//! Programs.

use crate::Guard;

/// A program.
pub trait Program<'a> {
    /// A guard against a program.
    type Guard: Guard;

    /// Returns bytecodes.
    fn bytecode(&'a self) -> Self::Guard;
}
