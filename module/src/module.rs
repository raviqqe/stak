//! Modules.

use crate::Guard;

/// A module.
pub trait Module<'a> {
    /// A guard against a module.
    type Guard: Guard;

    /// Returns bytecodes.
    fn bytecode(&'a self) -> Self::Guard;
}
