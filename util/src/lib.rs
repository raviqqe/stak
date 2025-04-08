//! Utilities around `libc`.

#![no_std]

mod heap;
mod mmap;

pub use heap::Heap;
pub use mmap::Mmap;
