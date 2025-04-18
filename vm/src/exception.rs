use crate::Error;
use core::error;

/// An exception.
pub trait Exception: From<Error> + error::Error {
    /// Returns `true` if an error is critical and not recoverable.
    fn is_critical(&self) -> bool;
}
