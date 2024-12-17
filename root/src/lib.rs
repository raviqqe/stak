#![doc = include_str!("../README.md")]

pub mod vm {
    //! A virtual machine and its runtime values.

    pub use stak_vm::*;
}

pub use vm::Vm;
