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

    fn current_jiffy(&self) -> Result<usize, Self::Error> {
        Ok(Default::default())
    }
}
