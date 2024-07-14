//! A minimal Stak Scheme interpreter.
//!
//! # Usage
//!
//! ```sh
//! mstak foo.scm
//! ```

#![no_std]
#![cfg_attr(not(test), no_main)]

stak_sac::libc_main!("main.scm");
