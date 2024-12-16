#![doc = include_str!("../README.md")]

#[doc(inline)]
pub use stak_vm::Vm;

pub mod vm {
    //! A virtual machine and its runtime values.

    pub use stak_vm::*;
}
