//! A Stak Scheme interpreter.
//!
//! # Usage
//!
//! ```sh
//! stak foo.scm
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
