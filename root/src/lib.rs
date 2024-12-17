#![doc = include_str!("../README.md")]
#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]

pub mod vm {
    //! A virtual machine and its runtime values.

    pub use stak_vm::*;
}

pub mod device {
    //! I/O devices.

    pub use stak_device::*;
}

pub mod file {
    //! File systems.

    pub use stak_file::*;
}

pub mod process_context {
    //! Process context.

    pub use stak_process_context::*;
}

pub mod r7rs {
    //! Primitives for R7RS Scheme.

    pub use stak_r7rs::*;
}

pub mod time {
    //! Time measurement.

    pub use stak_time::*;
}

pub use stak_macro::include_bytecode;
