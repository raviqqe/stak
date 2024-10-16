use super::Clock;
use core::{convert::Infallible, ptr::null_mut};
use stak_vm::Number;

/// A clock based on libc.
#[derive(Debug, Default)]
pub struct LibcClock {}

impl LibcClock {
    /// Creates a clock.
    pub const fn new() -> Self {
        Self {}
    }
}

impl Clock for LibcClock {
    type Error = Infallible;

    fn current_jiffy(&self) -> Result<Number, Self::Error> {
        Ok(Number::from_i64(
            unsafe { libc::time(null_mut()) } * 1_000_000_000,
        ))
    }
}
