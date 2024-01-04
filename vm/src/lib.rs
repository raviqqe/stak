//! A virtual machine and its runtime values.
//!
//! # Examples
//!
//! ```rust
//! use stak_code::{encode, Program};
//! use stak_device::FixedBufferDevice;
//! use stak_primitive::SmallPrimitiveSet;
//! use stak_vm::Vm;
//!
//! const BUFFER_SIZE: usize = 1 << 10;
//!
//! let mut heap = [Default::default(); 1 << 10];
//! let device = FixedBufferDevice::<BUFFER_SIZE, BUFFER_SIZE, BUFFER_SIZE>::new();
//! let mut vm = Vm::new(&mut heap, SmallPrimitiveSet::new(device)).unwrap();
//!
//! // Replace this with actual bytecodes of your program.
//! let program = encode(&Program::new(vec![], vec![]));
//!
//! vm.initialize(program).unwrap();
//! vm.run().unwrap();
//! ```

#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(any(feature = "std", feature = "trace", test))]
extern crate std;

mod cons;
mod error;
mod number;
mod primitive_set;
mod symbol_index;
mod r#type;
mod value;
mod vm;

pub use cons::Cons;
pub use error::Error;
pub use number::Number;
pub use primitive_set::PrimitiveSet;
pub use r#type::Type;
pub use value::Value;
pub use vm::Vm;
