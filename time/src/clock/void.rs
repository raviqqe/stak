use crate::Clock;
use core::convert::Infallible;
use stak_vm::Number;

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

    fn current_jiffy(&self) -> Result<Number, Self::Error> {
        Ok(Default::default())
    }
}
