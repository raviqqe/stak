use crate::Clock;
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
    fn current_jiffy(&self) -> Number {
        Default::default()
    }
}
