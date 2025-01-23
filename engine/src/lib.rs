//! Stak Scheme scripting engine for Rust.
//!
//! # Examples
//!
//! ```rust
//! use core::{
//!     error::Error,
//!     str::{self, FromStr},
//! };
//! use stak::{
//!     device::ReadWriteDevice,
//!     file::VoidFileSystem,
//!     include_module,
//!     module::{Module, UniversalModule},
//!     process_context::VoidProcessContext,
//!     r7rs::{SmallError, SmallPrimitiveSet},
//!     time::VoidClock,
//!     vm::Vm,
//! };
//!
//! const BUFFER_SIZE: usize = 1 << 8;
//! const HEAP_SIZE: usize = 1 << 16;
//!
//! static MODULE: UniversalModule = include_module!("fibonacci.scm");
//!
//! fn main() -> Result<(), Box<dyn Error>> {
//!     run(
//!         &MODULE.bytecode(),
//!         input.to_string().as_bytes(),
//!         &mut output,
//!         &mut error,
//!     )?;
//!
//!     // If stderr is not empty, we assume that some error has occurred.
//!     if !error.is_empty() {
//!         return Err(str::from_utf8(&error)?.into());
//!     }
//!
//!     // Decode and test the output.
//!     assert_eq!(isize::from_str(&str::from_utf8(&output)?)?, 610);
//!
//!     Ok(())
//! }
//!
//! fn run(
//!     bytecodes: &[u8],
//!     input: &[u8],
//!     output: &mut Vec<u8>,
//!     error: &mut Vec<u8>,
//! ) -> Result<(), SmallError> {
//!     let mut heap = [Default::default(); HEAP_SIZE];
//!     let mut vm = Vm::new(
//!         &mut heap,
//!         SmallPrimitiveSet::new(
//!             // Create and attach an in-memory I/O device.
//!             ReadWriteDevice::new(input, output, error),
//!             VoidFileSystem::new(),
//!             VoidProcessContext::new(),
//!             VoidClock::new(),
//!         ),
//!     )?;
//!
//!     vm.initialize(bytecodes.iter().copied())?;
//!     vm.run()
//! }
//! ```

#![no_std]

mod engine;
mod error;
mod primitive_set;

pub use engine::*;
pub use error::*;
