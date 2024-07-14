//! A Stak Scheme interpreter.
//!
//! # Usage
//!
//! ```sh
//! stak foo.scm
//! ```

#![no_std]
#![cfg_attr(not(test), no_main)]

stak_sac::main!("main.scm");
