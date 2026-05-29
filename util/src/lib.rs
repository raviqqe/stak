//! Utilities around `libc`.

#![no_std]

#[doc(hidden)]
pub mod __private {
    pub use noop_executor;
    pub use winter_maybe_async::maybe_await;
}

/// Blocks on a future if an `async` feature in on, or returns a given value as
/// it is otherwise.
#[macro_export]
macro_rules! block_on {
    ($value:expr) => {
        $crate::__private::noop_executor::block_on(async {
            $crate::__private::maybe_await!($value)
        })
    };
}
