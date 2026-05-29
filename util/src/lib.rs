//! Utilities around `libc`.

#![no_std]

#[doc(hidden)]
pub mod __private {
    pub use noop_executor;
    pub use winter_maybe_async::maybe_await;
}

/// Blocks on a value, awaiting it when asynchronous operations are compiled in
/// by `winter-maybe-async`, or returning it as is otherwise.
#[macro_export]
macro_rules! block_on {
    ($value:expr) => {
        $crate::__private::noop_executor::block_on(async {
            $crate::__private::maybe_await!($value)
        })
    };
}
