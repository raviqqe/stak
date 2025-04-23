//! Asynchronous contexts for Stak Scheme.

#![no_std]

extern crate alloc;
use alloc::boxed::Box;
use core::cell::Cell;
use stak_vm::{Error, Value};

/// An asynchronous context.
// TODO Allow injecting allocators when the allocator API is stabilized.
#[derive(Default)]
pub struct AsyncContext<'a> {
    future: Cell<Option<Box<dyn Future<Output = Value> + 'a>>>,
}

impl<'a> AsyncContext<'a> {
    /// Creates a context.
    pub fn new() -> Self {
        Self::default()
    }

    /// Takes a future.
    pub fn take_future(&self) -> Box<dyn Future<Output = Value> + 'a> {
        self.future.take().unwrap()
    }

    /// Yields a future.
    pub fn r#yield(&self, future: impl Future<Output = Value> + 'a) -> Result<(), Error> {
        self.future.set(Some(Box::new(future)));

        Err(Error::Yield)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use stak_vm::Number;

    #[tokio::test]
    async fn r#yield() {
        let context = AsyncContext::new();

        let error = context
            .r#yield(async { Number::from_i64(42).into() })
            .unwrap_err();

        assert_eq!(error, Error::Yield);

        let future = context.take_future();

        assert_eq!(Box::into_pin(future).await, Number::from_i64(42).into());
    }
}
