use super::Clock;
use std::time::{SystemTime, SystemTimeError, UNIX_EPOCH};

const NANOSECONDS_PER_SECOND: u64 = 1_000_000_000;

/// A clock provided by an operating system.
#[derive(Debug, Default)]
pub struct OsClock {}

impl OsClock {
    /// Creates a time.
    pub const fn new() -> Self {
        Self {}
    }
}

impl Clock for OsClock {
    type Error = SystemTimeError;

    fn current_jiffy(&self) -> Result<u64, Self::Error> {
        Ok(SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos() as _)
    }

    fn jiffies_per_second(&self) -> u64 {
        NANOSECONDS_PER_SECOND
    }
}
