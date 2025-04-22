//! Asynchronous context for Stak Scheme.

use bumpalo::Bump;

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
}
