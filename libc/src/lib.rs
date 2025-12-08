//! Utilities around `libc` for Stak Scheme.

#![no_std]

extern crate alloc;

mod mmap;

pub use mmap::Mmap;
