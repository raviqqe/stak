//! Modules.

use crate::Guard;

/// A module.
pub trait Module<'a> {
    /// A read guard against a module.
    type Guard: Guard;

    /// Returns bytecode.
    fn bytecode(&'a self) -> Self::Guard;
}
