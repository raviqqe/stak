//! Utilities around `libc` for Stak Scheme.

#![no_std]

mod heap;
mod mmap;

pub use heap::Heap;
pub use mmap::Mmap;
