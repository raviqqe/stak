//! A command to interpret a bytecode file.
//!
//! # Usage
//!
//! ```sh
//! stak-interpret foo.bc
//! ```

#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(all(not(feature = "std"), not(test)), no_main)]

#[cfg(not(feature = "std"))]
extern crate alloc;

mod options;

cfg_select! {
    feature = "std" => {
        mod os;

        use os::main;
    }
    _ => {
        mod libc;

        use libc::main;
    }
}

// Dependencies of the libc backend are unused when both backends are enabled.
#[cfg(all(feature = "libc", feature = "std"))]
use {dlmalloc as _, origin as _, stak_libc as _};
