use super::Clock;
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
    fn current_jiffy(&self) -> Number {
        Number::from_i64(libc::time() as _)
    }
}
