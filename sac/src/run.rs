#[cfg(feature = "libc")]
mod libc;
#[cfg(feature = "std")]
mod os;

#[cfg(feature = "libc")]
pub use libc::run_libc;
#[cfg(feature = "std")]
pub use os::run;
