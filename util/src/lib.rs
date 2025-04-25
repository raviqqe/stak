//! Utilities around `libc`.

#![no_std]

mod heap;
mod mmap;

pub use heap::Heap;
pub use mmap::Mmap;

#[doc(hidden)]
pub mod __private {
    pub use cfg_elif;
    pub use core;
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
            $crate::__private::core::convert::identity($value)
        })
    };
}
