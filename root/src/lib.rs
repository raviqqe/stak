#![doc = include_str!("../README.md")]

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

pub mod time {
    //! Time measurement.

    pub use stak_time::*;
}

pub use stak_build::*;
pub use stak_macro::*;
