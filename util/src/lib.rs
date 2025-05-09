//! Utilities around `libc`.

#![no_std]

#[doc(hidden)]
pub mod __private {
    pub use cfg_elif;
    pub use noop_executor;
}

/// Blocks on a future if an `async` feature in on, or returns a given value as
/// it is otherwise.
#[macro_export]
macro_rules! block_on {
    ($value:expr) => {
        $crate::__private::cfg_elif::expr::feature!(if ("async") {
            $crate::__private::noop_executor::block_on($value)
        } else {
            $value
        })
    };
}
