//! A process context for Stak Scheme.

#![no_std]

#[cfg(feature = "std")]
extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "std")]
mod os;
mod process_context;
mod void;

#[cfg(feature = "std")]
pub use os::OsProcessContext;
pub use process_context::ProcessContext;
pub use void::VoidProcessContext;
