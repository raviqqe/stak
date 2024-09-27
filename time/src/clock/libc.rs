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
    type Error = Never;

    fn current_jiffy(&self) -> Result<Number, Never> {
        Number::from_i64(libc::time() as _)
    }
}
