//! Asynchronous context for Stak Scheme.

use allocator_api2::{alloc::Allocator, boxed::Box};
use stak_vm::Value;

/// An asynchronous context.
#[derive(Debug)]
pub struct AsyncContext<A: Allocator, E> {
    allocator: A,
    r#yield: fn(Box<dyn Future<Output = Value>, A>) -> E,
}

impl<A: Allocator, E> AsyncContext<A, E> {
    /// Creates a context.
    pub fn new(allocator: A, r#yield: fn(Box<dyn Future<Output = Value>, A>) -> E) -> Self {
        Self { allocator, r#yield }
    }

    /// Yields a future.
    pub fn r#yield<T: Future<Output = Value>>(&self, future: T) -> Result<(), E> {
        let future = Box::<dyn Future<Output = Value>, A>::new_in(future, self.allocator);

        Err((self.r#yield)(future))
    }
}
