//! Stak Scheme scripting engine for Rust.
//!
//! # Examples
//!
//! ```rust
//! use any_fn::IntoAnyFn;
//! use core::error::Error;
//! use rand::random;
//! use stak::{
//!     engine::{Engine, EngineError},
//!     include_module,
//!     module::UniversalModule,
//! };
//!
//! const HEAP_SIZE: usize = 1 << 16;
//! const FOREIGN_VALUE_CAPACITY: usize = 1 << 10;
//!
//! struct Person {
//!     pies: usize,
//!     dodge: f64,
//!     wasted: bool,
//! }
//!
//! impl Person {
//!     pub fn new(pies: usize, dodge: f64) -> Self {
//!         Self {
//!             pies,
//!             dodge,
//!             wasted: false,
//!         }
//!     }
//!
//!     pub fn throw_pie(&mut self, other: &mut Person) {
//!         if self.wasted {
//!             return;
//!         }
//!
//!         self.pies -= 1;
//!
//!         if random::<f64>() > other.dodge {
//!             other.wasted = true;
//!         }
//!     }
//! }
//!
//! fn main() -> Result<(), Box<dyn Error>> {
//!     static MODULE: UniversalModule = include_module!("fight.scm");
//!
//!     run(&MODULE)?;
//!
//!     Ok(())
//! }
//!
//! fn run(module: &'static UniversalModule) -> Result<(), EngineError> {
//!     let mut heap = [Default::default(); HEAP_SIZE];
//!     let mut functions = [Person::new.into_any_fn(), Person::throw_pie.into_any_fn()];
//!     let mut engine = Engine::<FOREIGN_VALUE_CAPACITY>::new(&mut heap, &mut functions)?;
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
