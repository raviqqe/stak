use crate::Clock;
use core::convert::Infallible;

/// A void clock stopped a fixed time.
#[derive(Debug, Default)]
pub struct VoidClock {}

impl VoidClock {
    /// Creates a clock.
    pub const fn new() -> Self {
        Self {}
    }
}

impl Clock for VoidClock {
    type Error = Infallible;

    fn current_jiffy(&self) -> Result<u64, Self::Error> {
        Ok(Default::default())
    }

    fn jiffies_per_second(&self) -> u64 {
        1
    }
}
