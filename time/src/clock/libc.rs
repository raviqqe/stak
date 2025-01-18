use super::Clock;
use core::convert::Infallible;
use rustix::time::{clock_gettime, ClockId};
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
        let time = clock_gettime(ClockId::Realtime);

        // spell-checker: disable-next-line
        Ok(Number::from_i64(time.tv_sec * 1_000_000_000 + time.tv_nsec))
    }
}
