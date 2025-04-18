use super::Clock;
use core::convert::Infallible;
use rustix::time::{ClockId, clock_gettime};

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

    fn current_jiffy(&self) -> Result<u64, Self::Error> {
        let time = clock_gettime(ClockId::Realtime);

        // spell-checker: disable-next-line
        Ok((time.tv_sec * 1_000_000_000 + time.tv_nsec) as _)
    }
}
