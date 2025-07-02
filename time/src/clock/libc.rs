use super::Clock;
use core::convert::Infallible;
use rustix::time::{ClockId, clock_gettime};

const NANOSECONDS_PER_SECOND: u64 = 1_000_000_000;

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
        Ok(time.tv_sec as u64 * NANOSECONDS_PER_SECOND + time.tv_nsec as u64)
    }

    fn jiffies_per_second(&self) -> u64 {
        NANOSECONDS_PER_SECOND
    }
}
