use super::Clock;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// A clock provided by an OS.
#[derive(Debug, Default)]
pub struct OsClock {}

impl OsClock {
    /// Creates a time.
    pub fn new() -> Self {
        Self {}
    }
}

impl Clock for OsClock {
    fn current_jiffy(&self) -> Number {
        Number::from_i64(Duration::between(SystemTime::now(), UNIX_EPOCH) as _)
    }
}
