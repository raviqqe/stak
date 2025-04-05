use crate::Error;
use core::error;

/// A runtime error.
pub trait RuntimeError: From<Error> + error::Error {
    /// Returns `true` if an error is critical and not recoverable.
    fn is_critical(&self) -> bool;
}
