//! A command to compile a source file in Scheme into bytecodes for a virtual machine.
//!
//! # Usage
//!
//! ```sh
//! stak-decode < foo.scm > foo.bc
//! ```

sac::main!(std::env!("STAK_BYTECODE_FILE"));
