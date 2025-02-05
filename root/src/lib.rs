#![doc = include_str!("../README.md")]
#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]

pub mod device {
    //! I/O devices.
    //!
    //! ## Examples
    //!
    //! ### Communication via standard I/O
    //!
    //! You can pass in-memory standard input and output to Scheme scripts for
    //! communicate information between Rust and Scheme.
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

    pub use stak_device::*;
}

#[cfg(feature = "alloc")]
pub mod dynamic {
    //! Dynamically-defined primitives.

    pub use stak_dynamic::*;
}

#[cfg(feature = "alloc")]
pub mod engine {
    //! A scripting engine.

    pub use stak_engine::*;
}

pub mod file {
    //! File systems.

    pub use stak_file::*;
}

pub mod module {
    //! Modules.

    pub use stak_module::*;
}

pub mod process_context {
    //! Process context.

    pub use stak_process_context::*;
}

pub mod r7rs {
    //! Primitives for R7RS Scheme.

    pub use stak_r7rs::*;
}

pub mod sac {
    //! Standalone complex.

    pub use stak_sac::*;
}

pub mod time {
    //! Time measurement.

    pub use stak_time::*;
}

pub mod vm {
    //! A virtual machine and its runtime values.
    //!
    //! # Examples
    //!
    //! ## Embedding Scheme scripts in Rust with a custom virtual machine
    //!
    //! First, prepare a Scheme script named `src/hello.scm`.
    //!
    //! ```scheme
    //! (import (scheme base))
    //!
    //! (write-string "Hello, world!\n")
    //! ```
    //!
    //! Then, add a build script at `build.rs` to build the Scheme source file
    //! into bytecodes.
    //!
    //! ```rust no_run
    //! use stak_build::{build_r7rs, BuildError};
    //!
    //! fn main() -> Result<(), BuildError> {
    //!     build_r7rs()
    //! }
    //! ```
    //!
    //! Finally, you can include the Scheme script into a Rust program using
    //! [`stak::include_module`] macro and run the script.
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

    pub use stak_vm::*;
}

pub use stak_macro::include_module;
