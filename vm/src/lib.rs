//! A virtual machine and its runtime values.
//!
//! # Examples
//!
//! ```rust
//! use stak_device::FixedBufferDevice;
//! use stak_file::VoidFileSystem;
//! use stak_macro::compile_r7rs;
//! use stak_primitive::SmallPrimitiveSet;
//! use stak_process_context::VoidProcessContext;
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
//!     ),
//! ).unwrap();
//!
//! const PROGRAM: &[u8] = compile_r7rs!(r#"
//!     (import (scheme write))
//!
//!     (display "Hello, world!")
//! "#);
//!
//! vm.initialize(PROGRAM.iter().copied()).unwrap();
//! vm.run().unwrap();
//!
//! assert_eq!(vm.primitive_set().device().output(), b"Hello, world!");
//! ```

#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(any(feature = "std", feature = "trace_instruction", test))]
extern crate std;

mod cons;
mod error;
mod memory;
mod number;
mod primitive_set;
mod profiler;
mod stack_slot;
mod symbol_index;
mod r#type;
mod value;
mod vm;

pub use cons::{Cons, Tag};
pub use error::Error;
pub use number::Number;
pub use primitive_set::PrimitiveSet;
pub use profiler::Profiler;
pub use r#type::Type;
pub use stack_slot::StackSlot;
pub use value::Value;
pub use vm::Vm;
