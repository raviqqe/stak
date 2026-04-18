//! Utilities around `libc`.

#![no_std]

#[doc(hidden)]
pub mod __private {
    pub use noop_executor;
}

/// Blocks on a future if an `async` feature in on, or returns a given value as
/// it is otherwise.
#[macro_export]
macro_rules! block_on {
    ($value:expr) => {
        ::core::cfg_select! {
            feature = "async" => {
                $crate::__private::noop_executor::block_on($value)
            }
            _ => {
                $value
            }
        }
    };
}
