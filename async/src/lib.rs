//! Asynchronous context for Stak Scheme.

use bumpalo::Bump;
use stak_vm::Error;

/// An asynchronous context.
#[derive(Debug, Default)]
pub struct AsyncContext {
    allocator: Bump,
}

impl AsyncContext {
    /// Creates a context.
    pub fn new() -> Self {
        Self::default()
    }

    /// Yields a future.
    pub fn r#yield<T: Future>(&self, future: T) -> Result<(), Error> {
        Err(Error::Yield)
    }
}
