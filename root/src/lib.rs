#![doc = include_str!("../README.md")]
#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]

pub mod device {
    //! I/O devices.

    pub use stak_device::*;
}

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

pub mod native {
    //! Native functions and objects in Rust.

    pub use stak_native::*;
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

    pub use stak_vm::*;
}

pub use stak_macro::include_module;
