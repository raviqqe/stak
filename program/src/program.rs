//! Programs.

use crate::Guard;

/// A program.
pub trait Program<'a> {
    /// Returns bytecodes.
    fn bytecode(&'a self) -> impl Guard;
}
