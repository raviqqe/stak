use super::Clock;
use std::time::{SystemTime, SystemTimeError, UNIX_EPOCH};

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

    fn current_jiffy(&self) -> Result<usize, Self::Error> {
        Ok(SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos() as _)
    }
}
