//! A time for Stak Scheme.

#![no_std]

#[cfg(feature = "std")]
extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "libc")]
mod libc;
#[cfg(feature = "std")]
mod os;
mod primitive;
mod time;
mod void;

#[cfg(feature = "libc")]
pub use libc::LibcTime;
#[cfg(feature = "std")]
pub use os::OsTime;
pub use primitive::{Primitive, TimePrimitiveSet};
pub use time::Clock;
pub use void::VoidTime;
