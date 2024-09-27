#[cfg(feature = "libc")]
mod libc;
#[cfg(feature = "std")]
mod os;
mod void;

#[cfg(feature = "libc")]
pub use libc::LibcClock;
#[cfg(feature = "std")]
pub use os::OsClock;
use stak_vm::Number;
pub use void::VoidClock;

/// A clock.
pub trait Clock {
    /// Returns a current jiffy.
    fn current_jiffy(&self) -> Number;
}
