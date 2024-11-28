use super::Clock;
use stak_vm::Number;
use std::time::{SystemTime, SystemTimeError, UNIX_EPOCH};

/// A clock provided by an OS.
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

    fn current_jiffy(&self) -> Result<Number, Self::Error> {
        Ok(Number::from_i64(
            SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos() as _,
        ))
    }
}
