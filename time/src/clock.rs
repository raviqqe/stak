#[cfg(feature = "libc")]
mod libc;
#[cfg(feature = "std")]
mod os;
mod void;

use core::error::Error;
#[cfg(feature = "libc")]
pub use libc::LibcClock;
#[cfg(feature = "std")]
pub use os::OsClock;
pub use void::VoidClock;

/// A clock.
pub trait Clock {
    /// An error.
    type Error: Error;

    /// Returns a current jiffy.
    fn current_jiffy(&self) -> Result<u64, Self::Error>;
}
