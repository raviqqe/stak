//! Asynchronous context for Stak Scheme.

use allocator_api2::{alloc::Allocator, boxed::Box};
use core::mem::forget;
use core::pin::Pin;
use stak_vm::Value;

/// An asynchronous context.
#[derive(Debug)]
pub struct AsyncContext<'a, A: Allocator + 'a, E> {
    allocator: A,
    r#yield: fn(Pin<Box<dyn Future<Output = Value> + 'a, A>>) -> E,
}

impl<'a, A: Allocator + Copy + 'a, E> AsyncContext<'a, A, E> {
    /// Creates a context.
    pub fn new(
        allocator: A,
        r#yield: fn(Pin<Box<dyn Future<Output = Value> + 'a, A>>) -> E,
    ) -> Self {
        Self { allocator, r#yield }
    }

    /// Yields a future.
    pub fn r#yield(&self, mut value: impl Future<Output = Value> + 'a) -> Result<(), E> {
        let future: &mut (dyn Future<Output = Value> + 'a) = &mut value;
        let future: Box<dyn Future<Output = Value> + 'a, A> =
            unsafe { Box::from_raw_in(future, self.allocator) };

        forget(value);

        Err((self.r#yield)(future.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use allocator_api2::alloc::Global;
    use stak_vm::Number;

    #[tokio::test]
    async fn r#yield() {
        let context = AsyncContext::new(Global, |future| future);

        context.r#yield(async { Number::from_i64(42).into() });
    }
}
