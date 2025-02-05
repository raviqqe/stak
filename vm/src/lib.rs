//! A virtual machine and its runtime values.
//!
//! # Examples
//!
//! ```rust
//! use stak_device::FixedBufferDevice;
//! use stak_file::VoidFileSystem;
//! use stak_macro::compile_r7rs;
//! use stak_process_context::VoidProcessContext;
//! use stak_r7rs::SmallPrimitiveSet;
//! use stak_time::VoidClock;
//! use stak_vm::Vm;
//!
//! const HEAP_SIZE: usize = 1 << 16;
//! const BUFFER_SIZE: usize = 1 << 10;
//!
//! let mut heap = [Default::default(); HEAP_SIZE];
//! let device = FixedBufferDevice::<BUFFER_SIZE, 0>::new(&[]);
//! let mut vm = Vm::new(
//!     &mut heap,
//!     SmallPrimitiveSet::new(
//!         device,
//!         VoidFileSystem::new(),
//!         VoidProcessContext::new(),
//!         VoidClock::new(),
//!     ),
//! )
//! .unwrap();
//!
//! const BYTECODE: &[u8] = compile_r7rs!(
//!     r#"
//!     (import (scheme write))
//!
//!     (display "Hello, world!")
//!     "#
//! );
//!
//! vm.initialize(BYTECODE.iter().copied()).unwrap();
//! vm.run().unwrap();
//!
//! assert_eq!(vm.primitive_set().device().output(), b"Hello, world!");
//! ```
//!
//! ### Embedding Scheme scripts in Rust
//!
//! First, prepare a Scheme script at `src/hello.scm`.
//!
//! ```scheme
//! (import (scheme base))
//!
//! (write-string "Hello, world!\n")
//! ```
//!
//! Then, add a build script at `build.rs` to build the Scheme source file into
//! bytecodes.
//!
//! ```rust no_run
//! use stak_build::{build_r7rs, BuildError};
//!
//! fn main() -> Result<(), BuildError> {
//!     build_r7rs()
//! }
//! ```
//!
//! Now, you can include the Scheme script into a program in Rust using [the `stak::include_module` macro](https://docs.rs/stak/latest/stak/macro.include_module.html).
//!
//! ```rust
//! use core::error::Error;
//! use stak::{
//!     device::StdioDevice,
//!     file::VoidFileSystem,
//!     include_module,
//!     process_context::VoidProcessContext,
//!     module::{Module, UniversalModule},
//!     r7rs::{SmallError, SmallPrimitiveSet},
//!     time::VoidClock,
//!     vm::Vm,
//! };
//!
//! const HEAP_SIZE: usize = 1 << 16;
//!
//! // Include a Scheme script in the bytecode format built by the build script above.
//! static MODULE: UniversalModule = include_module!("hello.scm");
//!
//! fn main() -> Result<(), Box<dyn Error>> {
//!     run(&MODULE.bytecode())?;
//!
//!     Ok(())
//! }
//!
//! fn run(bytecodes: &[u8]) -> Result<(), SmallError> {
//!     // Prepare a heap memory of a virtual machine.
//!     let mut heap = [Default::default(); HEAP_SIZE];
//!     // Create a virtual machine with its heap memory primitive procedures.
//!     let mut vm = Vm::new(
//!         &mut heap,
//!         SmallPrimitiveSet::new(
//!             // Attach standard input, output, and error of this process to a virtual machine.
//!             StdioDevice::new(),
//!             // Use void system interfaces for security because we don't need them for this example.
//!             VoidFileSystem::new(),
//!             VoidProcessContext::new(),
//!             VoidClock::new(),
//!         ),
//!     )?;
//!
//!     // Initialize a virtual machine with bytecodes.
//!     vm.initialize(bytecodes.iter().copied())?;
//!     // Run bytecodes on a virtual machine.
//!     vm.run()
//! }
//! ```
//!
//! ### Communication between Scheme and Rust
//!
//! Currently, in-memory standard input (`stdin`) and output (`stdout`) to
//! Scheme scripts are the only way to communicate information between Rust
//! programs and Scheme scripts.
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
//!     let input = 15;
//!     let mut output = vec![];
//!     let mut error = vec![];
//!
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

#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]
#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(any(feature = "trace_instruction", test))]
extern crate std;

mod code;
mod cons;
mod error;
mod instruction;
mod memory;
mod number;
mod primitive_set;
mod profiler;
mod stack_slot;
mod r#type;
mod value;
mod vm;

pub use cons::{Cons, Tag};
pub use error::Error;
pub use memory::Memory;
pub use number::{Number, NumberRepresentation};
pub use primitive_set::PrimitiveSet;
pub use profiler::Profiler;
pub use r#type::Type;
pub use stack_slot::StackSlot;
pub use value::Value;
pub use vm::Vm;
