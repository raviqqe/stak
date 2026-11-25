//! A command to decode a bytecode file.
//!
//! It decodes a program in bytecode and prints it in Markdown. This command is
//! primarily for debugging.
//!
//! # Usage
//!
//! ```sh
//! stak-decode < foo.bc
//! ```

#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(all(not(feature = "std"), not(test)), no_main)]

cfg_select! {
    feature = "std" => {
        stak_sac::main!("main.scm");
    }
    _ => {
        stak_sac::libc_main!("main.scm");
    }
}
