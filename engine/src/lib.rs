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
//!     engine::{Engine, ScriptError},
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
//! static MODULE: UniversalModule = include_module!("main.scm");
//!
//! struct Person {
//!     pies: usize,
//!     dodge: f64,
//!     wasted: bool,
//! }
//!
//! fn main() -> Result<(), Box<dyn Error>> {
//!     run(&MODULE)?;
//!
//!     Ok(())
//! }
//!
//! fn run(module: &'static UniversalModule) -> Result<(), ScriptError> {
//!     let mut heap = [Default::default(); HEAP_SIZE];
//!     let mut engine = Engine::<0>::new(&mut heap, &mut [])?;
//!
//!     engine.run(module)
//! }
//! ```

#![no_std]

mod engine;
mod error;
mod primitive_set;

pub use engine::*;
pub use error::*;
