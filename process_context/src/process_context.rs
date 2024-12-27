#[cfg(feature = "libc")]
mod libc;
mod memory;
#[cfg(feature = "std")]
mod os;
mod void;

#[cfg(feature = "libc")]
pub use libc::LibcProcessContext;
pub use memory::MemoryProcessContext;
#[cfg(feature = "std")]
pub use os::OsProcessContext;
pub use void::VoidProcessContext;

/// A process context.
pub trait ProcessContext {
    /// Returns a command name and its arguments in a reverse order.
    fn command_line_rev(&self) -> impl IntoIterator<Item = &str>;

    /// Returns environment variables.
    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)>;
}
